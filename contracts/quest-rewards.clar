;; quest-rewards
;; Manages reward distribution and token economics for completed quests in the Quest Nest platform
;; This contract handles the creation, allocation, and distribution of QNEST tokens as rewards
;; for successfully completed quests, implementing the economic incentive structure of the platform.

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-QUEST (err u101))
(define-constant ERR-QUEST-NOT-COMPLETED (err u102))
(define-constant ERR-INVALID-REWARD-AMOUNT (err u103))
(define-constant ERR-INSUFFICIENT-FUNDS (err u104))
(define-constant ERR-REWARD-ALREADY-CLAIMED (err u105))
(define-constant ERR-STAKING-PERIOD-ACTIVE (err u106))
(define-constant ERR-INVALID-POOL (err u107))
(define-constant ERR-POOL-EMPTY (err u108))
(define-constant ERR-USER-NOT-FOUND (err u109))

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant MIN-REWARD-AMOUNT u1)
(define-constant MAX-REWARD-AMOUNT u1000000)
(define-constant DEFAULT-REWARD-AMOUNT u10)
(define-constant STAKING-DURATION-BLOCKS u144) ;; ~1 day at 10 min block time

;; Data maps and variables
;; Stores the total token supply of QNEST tokens
(define-data-var token-supply uint u0)

;; Tracks user balances of QNEST tokens
(define-map token-balances principal uint)

;; Stores quest details including reward amounts
(define-map quests
  uint
  {
    creator: principal,
    reward-amount: uint,
    is-completed: bool,
    reward-claimed: bool
  }
)

;; Maps for community reward pools
(define-map community-pools
  uint
  {
    total-amount: uint,
    active: bool
  }
)

;; Tracks user participation in community pools
(define-map pool-participants
  { pool-id: uint, user: principal }
  {
    contribution: uint,
    share-percentage: uint,
    reward-claimed: bool
  }
)

;; Tracks staked rewards and their unlock heights
(define-map staked-rewards
  { quest-id: uint, user: principal }
  {
    amount: uint,
    unlock-height: uint
  }
)

;; Private functions
(define-private (is-contract-owner)
  (is-eq tx-sender CONTRACT-OWNER)
)

(define-private (mint-tokens (amount uint) (recipient principal))
  (begin
    (var-set token-supply (+ (var-get token-supply) amount))
    (map-set token-balances recipient 
        (+ (default-to u0 (map-get? token-balances recipient)) amount))
    (ok amount)
  )
)

(define-private (transfer-tokens (amount uint) (sender principal) (recipient principal))
  (let (
    (sender-balance (default-to u0 (map-get? token-balances sender)))
  )
    (asserts! (>= sender-balance amount) ERR-INSUFFICIENT-FUNDS)
    (map-set token-balances sender (- sender-balance amount))
    (map-set token-balances recipient 
        (+ (default-to u0 (map-get? token-balances recipient)) amount))
    (ok amount)
  )
)

(define-private (calculate-reward (quest-id uint))
  (let (
    (quest (unwrap! (map-get? quests quest-id) ERR-INVALID-QUEST))
    (reward-amount (get reward-amount quest))
  )
    (if (> reward-amount u0)
      reward-amount
      DEFAULT-REWARD-AMOUNT)
  )
)

;; Read-only functions
(define-read-only (get-token-supply)
  (var-get token-supply)
)

(define-read-only (get-balance (user principal))
  (default-to u0 (map-get? token-balances user))
)

(define-read-only (get-quest (quest-id uint))
  (map-get? quests quest-id)
)

(define-read-only (get-community-pool (pool-id uint))
  (map-get? community-pools pool-id)
)

(define-read-only (get-staked-reward (quest-id uint) (user principal))
  (map-get? staked-rewards { quest-id: quest-id, user: user })
)

(define-read-only (is-reward-claimable (quest-id uint))
  (let (
    (quest (unwrap! (map-get? quests quest-id) ERR-INVALID-QUEST))
  )
    (and 
      (get is-completed quest)
      (not (get reward-claimed quest)))
  )
)

(define-read-only (get-pool-participant-info (pool-id uint) (user principal))
  (map-get? pool-participants { pool-id: pool-id, user: user })
)

;; Public functions
;; Create a new quest with specified reward amount
(define-public (create-quest (quest-id uint) (reward-amount uint))
  (begin
    ;; Validate the reward amount
    (asserts! (and (>= reward-amount MIN-REWARD-AMOUNT) 
                   (<= reward-amount MAX-REWARD-AMOUNT)) 
              ERR-INVALID-REWARD-AMOUNT)
    
    ;; Create the quest with the creator's information
    (map-set quests quest-id {
      creator: tx-sender,
      reward-amount: reward-amount,
      is-completed: false,
      reward-claimed: false
    })
    
    ;; Deduct the reward amount from the creator's balance and stake it
    (unwrap! (transfer-tokens reward-amount tx-sender (as-contract tx-sender)) ERR-INSUFFICIENT-FUNDS)
    
    ;; Set staking information
    (map-set staked-rewards { quest-id: quest-id, user: tx-sender } {
      amount: reward-amount,
      unlock-height: (+ block-height STAKING-DURATION-BLOCKS)
    })
    
    (ok quest-id)
  )
)

;; Mark a quest as completed, which makes it eligible for reward claiming
(define-public (complete-quest (quest-id uint))
  (let (
    (quest (unwrap! (map-get? quests quest-id) ERR-INVALID-QUEST))
  )
    ;; Only the creator of the quest can mark it as completed
    (asserts! (is-eq (get creator quest) tx-sender) ERR-NOT-AUTHORIZED)
    
    ;; Update the quest status to completed
    (map-set quests quest-id (merge quest { is-completed: true }))
    
    (ok true)
  )
)

;; Claim reward for a completed quest
(define-public (claim-reward (quest-id uint))
  (let (
    (quest (unwrap! (map-get? quests quest-id) ERR-INVALID-QUEST))
    (reward-amount (get reward-amount quest))
  )
    ;; Verify the quest is completed and reward not claimed yet
    (asserts! (get is-completed quest) ERR-QUEST-NOT-COMPLETED)
    (asserts! (not (get reward-claimed quest)) ERR-REWARD-ALREADY-CLAIMED)
    
    ;; Update quest to reflect claimed reward
    (map-set quests quest-id (merge quest { reward-claimed: true }))
    
    ;; Transfer the reward to the caller
    (transfer-tokens reward-amount (as-contract tx-sender) tx-sender)
    
    (ok reward-amount)
  )
)

;; Create a community reward pool
(define-public (create-community-pool (pool-id uint) (initial-amount uint))
  (begin
    (asserts! (>= initial-amount MIN-REWARD-AMOUNT) ERR-INVALID-REWARD-AMOUNT)
    (asserts! (not (default-to false (get active (map-get? community-pools pool-id)))) ERR-INVALID-POOL)
    
    ;; Transfer tokens to the pool
    (unwrap! (transfer-tokens initial-amount tx-sender (as-contract tx-sender)) ERR-INSUFFICIENT-FUNDS)
    
    ;; Create the pool
    (map-set community-pools pool-id {
      total-amount: initial-amount,
      active: true
    })
    
    ;; Register creator as a participant
    (map-set pool-participants { pool-id: pool-id, user: tx-sender } {
      contribution: initial-amount,
      share-percentage: u100, ;; Initial contributor gets 100%
      reward-claimed: false
    })
    
    (ok pool-id)
  )
)

;; Contribute to an existing community pool
(define-public (contribute-to-pool (pool-id uint) (amount uint))
  (let (
    (pool (unwrap! (map-get? community-pools pool-id) ERR-INVALID-POOL))
    (current-total (get total-amount pool))
    (participant-info (map-get? pool-participants { pool-id: pool-id, user: tx-sender }))
    (previous-contribution (default-to u0 (get contribution participant-info)))
  )
    ;; Verify pool is active
    (asserts! (get active pool) ERR-INVALID-POOL)
    (asserts! (>= amount MIN-REWARD-AMOUNT) ERR-INVALID-REWARD-AMOUNT)
    
    ;; Transfer contribution
    (unwrap! (transfer-tokens amount tx-sender (as-contract tx-sender)) ERR-INSUFFICIENT-FUNDS)
    
    ;; Update pool total
    (map-set community-pools pool-id (merge pool { total-amount: (+ current-total amount) }))
    
    ;; Update participant info
    (map-set pool-participants { pool-id: pool-id, user: tx-sender } {
      contribution: (+ previous-contribution amount),
      share-percentage: (/ (* (+ previous-contribution amount) u100) (+ current-total amount)),
      reward-claimed: false
    })
    
    (ok true)
  )
)

;; Claim rewards from a community pool based on participation share
(define-public (claim-pool-reward (pool-id uint))
  (let (
    (pool (unwrap! (map-get? community-pools pool-id) ERR-INVALID-POOL))
    (participant (unwrap! (map-get? pool-participants { pool-id: pool-id, user: tx-sender }) ERR-USER-NOT-FOUND))
    (total-amount (get total-amount pool))
    (share (get share-percentage participant))
    (reward-amount (/ (* total-amount share) u100))
  )
    ;; Verify pool has funds and participant hasn't claimed
    (asserts! (> total-amount u0) ERR-POOL-EMPTY)
    (asserts! (not (get reward-claimed participant)) ERR-REWARD-ALREADY-CLAIMED)
    
    ;; Mark as claimed
    (map-set pool-participants { pool-id: pool-id, user: tx-sender }
      (merge participant { reward-claimed: true }))
    
    ;; Transfer the reward
    (transfer-tokens reward-amount (as-contract tx-sender) tx-sender)
    
    (ok reward-amount)
  )
)

;; Unstake tokens from a quest if the staking period has passed
(define-public (unstake-reward (quest-id uint))
  (let (
    (staked-info (unwrap! (map-get? staked-rewards { quest-id: quest-id, user: tx-sender }) ERR-INVALID-QUEST))
    (amount (get amount staked-info))
    (unlock-height (get unlock-height staked-info))
  )
    ;; Verify staking period is over
    (asserts! (>= block-height unlock-height) ERR-STAKING-PERIOD-ACTIVE)
    
    ;; Clear staking record
    (map-delete staked-rewards { quest-id: quest-id, user: tx-sender })
    
    ;; Return staked amount to user
    (transfer-tokens amount (as-contract tx-sender) tx-sender)
    
    (ok amount)
  )
)

;; Administrative function to mint new tokens (restricted to contract owner)
(define-public (mint (amount uint) (recipient principal))
  (begin
    (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
    (asserts! (> amount u0) ERR-INVALID-REWARD-AMOUNT)
    (mint-tokens amount recipient)
  )
)

;; Administrative function to close a community pool (restricted to contract owner)
(define-public (close-pool (pool-id uint))
  (let (
    (pool (unwrap! (map-get? community-pools pool-id) ERR-INVALID-POOL))
  )
    (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
    (map-set community-pools pool-id (merge pool { active: false }))
    (ok true)
  )
)