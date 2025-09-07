;; MediaForge Protocol - Dynamic Content Licensing & Creator Economy Platform

;; === ERROR CODES ===
(define-constant ERR-UNAUTHORIZED (err u1001))
(define-constant ERR-INSUFFICIENT-FUNDS (err u1002))
(define-constant ERR-INVALID-INPUT (err u1003))
(define-constant ERR-PROTOCOL-FROZEN (err u1004))
(define-constant ERR-COLLATERAL-SHORTAGE (err u1005))
(define-constant ERR-REPUTATION-TOO-LOW (err u1006))
(define-constant ERR-MARKET-INSTABILITY (err u1007))
(define-constant ERR-EMERGENCY-HALT (err u1008))
(define-constant ERR-LICENSE-NOT-FOUND (err u1009))
(define-constant ERR-CREATOR-NOT-REGISTERED (err u1010))
(define-constant ERR-GOVERNANCE-LOCKED (err u1011))
(define-constant ERR-PROPOSAL-INVALID (err u1012))
(define-constant ERR-STRATEGY-UNSUPPORTED (err u1013))
(define-constant ERR-COOLDOWN-ACTIVE (err u1014))

;; === PROTOCOL PARAMETERS ===
(define-constant DEPLOYER tx-sender)
(define-constant MINIMUM-COLLATERAL-RATIO u200) ;; 200%
(define-constant REPUTATION-CAP u10000)
(define-constant INSTABILITY-LIMIT u750) ;; 7.5%
(define-constant EMERGENCY-THRESHOLD u1500) ;; 15%
(define-constant GOVERNANCE-DELAY u2016) ;; ~2 weeks in blocks
(define-constant MIN-LICENSE-VALUE u500)
(define-constant COOLDOWN-BLOCKS u144) ;; ~24 hours

;; === STATE VARIABLES ===
(define-data-var protocol-active bool true)
(define-data-var forge-token-supply uint u0)
(define-data-var media-token-supply uint u0)
(define-data-var licensing-token-supply uint u0)
(define-data-var market-instability uint u0)
(define-data-var adaptive-pricing bool true)
(define-data-var emergency-mode bool false)
(define-data-var last-market-check uint u0)
(define-data-var collateral-multiplier uint u200)
(define-data-var protocol-guardian (optional principal) none)
(define-data-var governance-delay-end uint u0)
(define-data-var license-registry-counter uint u1)
(define-data-var proposal-counter uint u1)

;; === DATA STRUCTURES ===
(define-map creator-profiles principal 
  {
    reputation: uint,
    total-staked: uint,
    stake-start: uint,
    last-activity: uint,
    verified: bool
  })

(define-map token-balances-forge principal uint)
(define-map token-balances-media principal uint)
(define-map token-balances-licensing principal uint)

(define-map collateral-positions principal 
  {
    locked-amount: uint,
    minted-tokens: uint,
    collateral-ratio: uint,
    liquidation-price: uint
  })

(define-map license-portfolios uint 
  {
    creator: principal,
    value: uint,
    investment-strategy: (string-ascii 64),
    last-yield-claim: uint,
    annual-yield: uint,
    portfolio-created: uint,
    active: bool
  })

(define-map creator-license-count principal uint)

(define-map market-sentiment-data uint 
  {
    instability-level: uint,
    recorded-at: uint,
    total-value-locked: uint,
    price-deviation: uint
  })

(define-map dao-proposals uint 
  {
    author: principal,
    proposal-text: (string-ascii 512),
    affirmative-votes: uint,
    negative-votes: uint,
    proposal-executed: bool,
    created-block: uint,
    voting-ends: uint,
    execution-delay: uint
  })

(define-map creator-voting-weight principal uint)
(define-map investment-strategies (string-ascii 64) bool)
(define-map creator-cooldowns principal uint)

;; === UTILITY FUNCTIONS ===
(define-private (is-deployer)
  (is-eq tx-sender DEPLOYER))

(define-private (is-guardian)
  (match (var-get protocol-guardian)
    admin (is-eq tx-sender admin)
    false))

(define-private (has-admin-privileges)
  (or (is-deployer) (is-guardian)))

(define-private (valid-amount (amount uint))
  (> amount u0))

(define-private (protocol-operational)
  (and 
    (var-get protocol-active)
    (not (var-get emergency-mode))))

(define-private (strategy-approved (strategy (string-ascii 64)))
  (default-to false (map-get? investment-strategies strategy)))

(define-private (get-smaller (x uint) (y uint))
  (if (<= x y) x y))

(define-private (cooldown-expired (creator principal))
  (let ((last-cooldown (default-to u0 (map-get? creator-cooldowns creator))))
    (>= block-height (+ last-cooldown COOLDOWN-BLOCKS))))

;; === REPUTATION & SCORING ===
(define-private (compute-reputation (creator principal))
  (let (
    (profile (default-to 
      {reputation: u0, total-staked: u0, stake-start: u0, last-activity: u0, verified: false}
      (map-get? creator-profiles creator)))
    (voting-power (default-to u0 (map-get? creator-voting-weight creator)))
    (base-points u50)
    (stake-bonus (/ (get total-staked profile) u2000))
    (longevity-bonus (/ (- block-height (get stake-start profile)) u500))
    (governance-bonus (/ voting-power u20))
    (verification-bonus (if (get verified profile) u100 u0))
    (final-score (+ base-points stake-bonus longevity-bonus governance-bonus verification-bonus))
  )
  (get-smaller final-score REPUTATION-CAP)))

;; === MARKET ANALYSIS ===
(define-private (assess-market-conditions)
  (let (
    (current-block block-height)
    (previous-check (var-get last-market-check))
    (time-elapsed (- current-block previous-check))
  )
  (if (> time-elapsed u50)
    (let (
      (instability-increase u25)
      (new-instability (+ (var-get market-instability) instability-increase))
    )
    (var-set market-instability new-instability)
    (var-set last-market-check current-block)
    (if (> new-instability EMERGENCY-THRESHOLD)
      (var-set emergency-mode true)
      true))
    true)))

;; === DYNAMIC COLLATERAL CALCULATION ===
(define-private (calculate-collateral-requirement (creator principal))
  (let (
    (reputation (compute-reputation creator))
    (base-multiplier (var-get collateral-multiplier))
    (market-volatility (var-get market-instability))
    (reputation-discount (/ (* reputation u30) REPUTATION-CAP))
    (volatility-premium (/ market-volatility u5))
    (adjusted-ratio (+ (- base-multiplier reputation-discount) volatility-premium))
  )
  (get-smaller (if (>= adjusted-ratio u120) adjusted-ratio u120) u300))) ;; Between 120% and 300%

;; === ADMINISTRATIVE FUNCTIONS ===
(define-public (assign-guardian (new-guardian principal))
  (begin
    (asserts! (is-deployer) ERR-UNAUTHORIZED)
    (var-set protocol-guardian (some new-guardian))
    (ok true)))

(define-public (freeze-protocol)
  (begin
    (asserts! (has-admin-privileges) ERR-UNAUTHORIZED)
    (var-set protocol-active false)
    (ok true)))

(define-public (unfreeze-protocol)
  (begin
    (asserts! (is-deployer) ERR-UNAUTHORIZED)
    (var-set protocol-active true)
    (var-set emergency-mode false)
    (ok true)))

(define-public (adjust-collateral-multiplier (new-multiplier uint))
  (begin
    (asserts! (is-deployer) ERR-UNAUTHORIZED)
    (asserts! (>= new-multiplier u100) ERR-INVALID-INPUT)
    (asserts! (is-eq (var-get governance-delay-end) u0) ERR-GOVERNANCE-LOCKED)
    (var-set collateral-multiplier new-multiplier)
    (ok true)))

(define-public (trigger-emergency-halt)
  (begin
    (asserts! (has-admin-privileges) ERR-UNAUTHORIZED)
    (var-set emergency-mode true)
    (var-set protocol-active false)
    (ok true)))

(define-public (register-strategy (strategy (string-ascii 64)))
  (begin
    (asserts! (is-deployer) ERR-UNAUTHORIZED)
    (map-set investment-strategies strategy true)
    (ok true)))

(define-public (deregister-strategy (strategy (string-ascii 64)))
  (begin
    (asserts! (is-deployer) ERR-UNAUTHORIZED)
    (map-delete investment-strategies strategy)
    (ok true)))

;; === CORE TOKEN MECHANICS ===
(define-public (mint-forge-tokens (collateral-amount uint))
  (let (
    (creator tx-sender)
    (reputation (compute-reputation creator))
    (required-ratio (calculate-collateral-requirement creator))
    (mintable-amount (/ (* collateral-amount u100) required-ratio))
  )
  (asserts! (protocol-operational) ERR-PROTOCOL-FROZEN)
  (asserts! (valid-amount collateral-amount) ERR-INVALID-INPUT)
  (asserts! (>= collateral-amount (* mintable-amount required-ratio)) ERR-COLLATERAL-SHORTAGE)
  (asserts! (cooldown-expired creator) ERR-COOLDOWN-ACTIVE)
  
  (assess-market-conditions)
  
  ;; Update balances and positions
  (map-set token-balances-forge creator 
           (+ (default-to u0 (map-get? token-balances-forge creator)) mintable-amount))
  (map-set collateral-positions creator 
           {
             locked-amount: collateral-amount,
             minted-tokens: mintable-amount,
             collateral-ratio: required-ratio,
             liquidation-price: (/ (* collateral-amount u80) u100)
           })
  (var-set forge-token-supply (+ (var-get forge-token-supply) mintable-amount))
  (map-set creator-cooldowns creator block-height)
  
  (ok mintable-amount)))

(define-public (redeem-forge-tokens (token-amount uint))
  (let (
    (creator tx-sender)
    (current-balance (default-to u0 (map-get? token-balances-forge creator)))
    (position (map-get? collateral-positions creator))
  )
  (asserts! (protocol-operational) ERR-PROTOCOL-FROZEN)
  (asserts! (valid-amount token-amount) ERR-INVALID-INPUT)
  (asserts! (>= current-balance token-amount) ERR-INSUFFICIENT-FUNDS)
  (asserts! (is-some position) ERR-CREATOR-NOT-REGISTERED)
  
  (let (
    (position-data (unwrap! position ERR-CREATOR-NOT-REGISTERED))
    (collateral-return (/ (* token-amount (get locked-amount position-data)) 
                         (get minted-tokens position-data)))
  )
  (map-set token-balances-forge creator (- current-balance token-amount))
  (var-set forge-token-supply (- (var-get forge-token-supply) token-amount))
  
  (ok collateral-return))))

(define-public (stake-media-tokens (amount uint))
  (let (
    (creator tx-sender)
    (current-balance (default-to u0 (map-get? token-balances-media creator)))
    (current-profile (default-to 
      {reputation: u0, total-staked: u0, stake-start: u0, last-activity: u0, verified: false}
      (map-get? creator-profiles creator)))
  )
  (asserts! (protocol-operational) ERR-PROTOCOL-FROZEN)
  (asserts! (valid-amount amount) ERR-INVALID-INPUT)
  (asserts! (>= current-balance amount) ERR-INSUFFICIENT-FUNDS)
  
  (map-set token-balances-media creator (- current-balance amount))
  (map-set creator-profiles creator 
           {
             reputation: (get reputation current-profile),
             total-staked: (+ (get total-staked current-profile) amount),
             stake-start: (if (is-eq (get stake-start current-profile) u0) 
                            block-height 
                            (get stake-start current-profile)),
             last-activity: block-height,
             verified: (get verified current-profile)
           })
  
  ;; Recalculate and update reputation
  (let ((updated-profile (unwrap-panic (map-get? creator-profiles creator))))
    (map-set creator-profiles creator 
             (merge updated-profile {reputation: (compute-reputation creator)})))
  
  (ok true)))

(define-public (unstake-media-tokens (amount uint))
  (let (
    (creator tx-sender)
    (current-balance (default-to u0 (map-get? token-balances-media creator)))
    (profile-data (map-get? creator-profiles creator))
  )
  (asserts! (protocol-operational) ERR-PROTOCOL-FROZEN)
  (asserts! (valid-amount amount) ERR-INVALID-INPUT)
  (asserts! (is-some profile-data) ERR-CREATOR-NOT-REGISTERED)
  (asserts! (cooldown-expired creator) ERR-COOLDOWN-ACTIVE)
  
  (let (
    (profile (unwrap! profile-data ERR-CREATOR-NOT-REGISTERED))
    (total-staked (get total-staked profile))
  )
  (asserts! (>= total-staked amount) ERR-INSUFFICIENT-FUNDS)
  
  (map-set token-balances-media creator (+ current-balance amount))
  (map-set creator-profiles creator 
           (merge profile {
             total-staked: (- total-staked amount),
             last-activity: block-height
           }))
  (map-set creator-cooldowns creator block-height)
  
  ;; Update reputation after unstaking
  (map-set creator-profiles creator 
           (merge (unwrap-panic (map-get? creator-profiles creator))
                  {reputation: (compute-reputation creator)}))
  
  (ok true))))

;; === LICENSE PORTFOLIO SYSTEM ===
(define-public (create-license-portfolio (initial-investment uint) (strategy (string-ascii 64)))
  (let (
    (creator tx-sender)
    (portfolio-id (var-get license-registry-counter))
    (creator-balance (default-to u0 (map-get? token-balances-forge creator)))
  )
  (asserts! (protocol-operational) ERR-PROTOCOL-FROZEN)
  (asserts! (valid-amount initial-investment) ERR-INVALID-INPUT)
  (asserts! (>= initial-investment MIN-LICENSE-VALUE) ERR-INVALID-INPUT)
  (asserts! (>= creator-balance initial-investment) ERR-INSUFFICIENT-FUNDS)
  (asserts! (strategy-approved strategy) ERR-STRATEGY-UNSUPPORTED)
  
  ;; Transfer tokens and create portfolio
  (map-set token-balances-forge creator (- creator-balance initial-investment))
  (map-set license-portfolios portfolio-id 
           {
             creator: creator,
             value: initial-investment,
             investment-strategy: strategy,
             last-yield-claim: block-height,
             annual-yield: u0,
             portfolio-created: block-height,
             active: true
           })
  
  ;; Update creator's portfolio count
  (map-set creator-license-count creator 
           (+ (default-to u0 (map-get? creator-license-count creator)) u1))
  
  ;; Increment global counter
  (var-set license-registry-counter (+ portfolio-id u1))
  
  (ok portfolio-id)))

(define-public (add-to-portfolio (portfolio-id uint) (additional-amount uint))
  (let (
    (creator tx-sender)
    (creator-balance (default-to u0 (map-get? token-balances-forge creator)))
    (portfolio-data (map-get? license-portfolios portfolio-id))
  )
  (asserts! (protocol-operational) ERR-PROTOCOL-FROZEN)
  (asserts! (valid-amount additional-amount) ERR-INVALID-INPUT)
  (asserts! (>= creator-balance additional-amount) ERR-INSUFFICIENT-FUNDS)
  (asserts! (is-some portfolio-data) ERR-LICENSE-NOT-FOUND)
  
  (let (
    (portfolio (unwrap! portfolio-data ERR-LICENSE-NOT-FOUND))
  )
  (asserts! (is-eq (get creator portfolio) creator) ERR-UNAUTHORIZED)
  (asserts! (get active portfolio) ERR-LICENSE-NOT-FOUND)
  
  ;; Transfer funds and update portfolio
  (map-set token-balances-forge creator (- creator-balance additional-amount))
  (map-set license-portfolios portfolio-id 
           (merge portfolio {value: (+ (get value portfolio) additional-amount)}))
  
  (ok true))))

(define-public (withdraw-from-portfolio (portfolio-id uint) (withdrawal-amount uint))
  (let (
    (creator tx-sender)
    (portfolio-data (map-get? license-portfolios portfolio-id))
  )
  (asserts! (protocol-operational) ERR-PROTOCOL-FROZEN)
  (asserts! (valid-amount withdrawal-amount) ERR-INVALID-INPUT)
  (asserts! (is-some portfolio-data) ERR-LICENSE-NOT-FOUND)
  
  (let (
    (portfolio (unwrap! portfolio-data ERR-LICENSE-NOT-FOUND))
    (portfolio-value (get value portfolio))
    (creator-balance (default-to u0 (map-get? token-balances-forge creator)))
  )
  (asserts! (is-eq (get creator portfolio) creator) ERR-UNAUTHORIZED)
  (asserts! (>= portfolio-value withdrawal-amount) ERR-INSUFFICIENT-FUNDS)
  
  ;; Execute withdrawal
  (map-set token-balances-forge creator (+ creator-balance withdrawal-amount))
  (map-set license-portfolios portfolio-id 
           (merge portfolio {value: (- portfolio-value withdrawal-amount)}))
  
  (ok true))))

(define-public (optimize-portfolio (portfolio-id uint))
  (let (
    (creator tx-sender)
    (portfolio-data (map-get? license-portfolios portfolio-id))
  )
  (asserts! (protocol-operational) ERR-PROTOCOL-FROZEN)
  (asserts! (is-some portfolio-data) ERR-LICENSE-NOT-FOUND)
  
  (let (
    (portfolio (unwrap! portfolio-data ERR-LICENSE-NOT-FOUND))
    (time-since-last-claim (- block-height (get last-yield-claim portfolio)))
    (calculated-yield (/ time-since-last-claim u50))
  )
  (asserts! (is-eq (get creator portfolio) creator) ERR-UNAUTHORIZED)
  (asserts! (> time-since-last-claim COOLDOWN-BLOCKS) ERR-COOLDOWN-ACTIVE)
  
  (map-set license-portfolios portfolio-id 
           (merge portfolio {
             last-yield-claim: block-height,
             annual-yield: calculated-yield
           }))
  
  (ok calculated-yield))))

;; === GOVERNANCE SYSTEM ===
(define-public (submit-dao-proposal (description (string-ascii 512)))
  (let (
    (creator tx-sender)
    (proposal-id (var-get proposal-counter))
    (voting-power (default-to u0 (map-get? creator-voting-weight creator)))
  )
  (asserts! (protocol-operational) ERR-PROTOCOL-FROZEN)
  (asserts! (> voting-power u0) ERR-UNAUTHORIZED)
  (asserts! (> (len description) u0) ERR-PROPOSAL-INVALID)
  
  (map-set dao-proposals proposal-id {
    author: creator,
    proposal-text: description,
    affirmative-votes: u0,
    negative-votes: u0,
    proposal-executed: false,
    created-block: block-height,
    voting-ends: (+ block-height GOVERNANCE-DELAY),
    execution-delay: (+ block-height (* GOVERNANCE-DELAY u2))
  })
  
  (var-set proposal-counter (+ proposal-id u1))
  
  (ok proposal-id)))

(define-public (cast-vote (proposal-id uint) (support bool))
  (let (
    (voter tx-sender)
    (voter-weight (default-to u0 (map-get? creator-voting-weight voter)))
    (proposal-data (map-get? dao-proposals proposal-id))
  )
  (asserts! (protocol-operational) ERR-PROTOCOL-FROZEN)
  (asserts! (> voter-weight u0) ERR-UNAUTHORIZED)
  (asserts! (is-some proposal-data) ERR-PROPOSAL-INVALID)
  
  (let (
    (proposal (unwrap! proposal-data ERR-PROPOSAL-INVALID))
  )
  (asserts! (< block-height (get voting-ends proposal)) ERR-GOVERNANCE-LOCKED)
  (asserts! (not (get proposal-executed proposal)) ERR-PROPOSAL-INVALID)
  
  (if support
    (map-set dao-proposals proposal-id 
             (merge proposal {affirmative-votes: (+ (get affirmative-votes proposal) voter-weight)}))
    (map-set dao-proposals proposal-id 
             (merge proposal {negative-votes: (+ (get negative-votes proposal) voter-weight)})))
  
  (ok true))))

;; === READ-ONLY QUERY FUNCTIONS ===
(define-read-only (get-forge-balance (creator principal))
  (default-to u0 (map-get? token-balances-forge creator)))

(define-read-only (get-media-balance (creator principal))
  (default-to u0 (map-get? token-balances-media creator)))

(define-read-only (get-creator-reputation (creator principal))
  (compute-reputation creator))

(define-read-only (get-protocol-metrics)
  {
    active: (var-get protocol-active),
    emergency-mode: (var-get emergency-mode),
    market-instability: (var-get market-instability),
    forge-supply: (var-get forge-token-supply),
    media-supply: (var-get media-token-supply)
  })

(define-read-only (get-portfolio-details (portfolio-id uint))
  (map-get? license-portfolios portfolio-id))

(define-read-only (get-collateral-position (creator principal))
  (map-get? collateral-positions creator))

(define-read-only (get-dao-proposal-info (proposal-id uint))
  (map-get? dao-proposals proposal-id))

(define-read-only (calculate-required-collateral (creator principal))
  (calculate-collateral-requirement creator))

(define-read-only (get-creator-profile (creator principal))
  (map-get? creator-profiles creator))