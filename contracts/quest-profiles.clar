;; quest-profiles
;; 
;; This contract creates a comprehensive profile system for Quest Nest users,
;; tracking their quest participation history, completion rates, verification 
;; contributions, and overall platform reputation. Users build a verifiable 
;; portfolio of achievements over time, showcasing their personal development journey.
;; The profile system includes customizable privacy settings, allowing users to 
;; control what aspects of their quest history are public while still maintaining 
;; the verifiable nature of their achievements on the blockchain.

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-USER-ALREADY-EXISTS (err u101))
(define-constant ERR-USER-NOT-FOUND (err u102))
(define-constant ERR-INVALID-PRIVACY-SETTING (err u103))
(define-constant ERR-INVALID-REPUTATION-UPDATE (err u104))
(define-constant ERR-INVALID-ACHIEVEMENT (err u105))
(define-constant ERR-VERIFICATION-NOT-FOUND (err u106))

;; Privacy settings constants
(define-constant PRIVACY-PUBLIC u1)
(define-constant PRIVACY-FRIENDS-ONLY u2)
(define-constant PRIVACY-PRIVATE u3)

;; Data structures

;; User profiles storage
(define-map user-profiles
  { user: principal }
  {
    display-name: (string-ascii 50),
    bio: (string-utf8 500),
    joined-at: uint,
    reputation-score: uint,
    quests-completed: uint,
    quests-verified: uint,
    privacy-setting: uint
  }
)

;; User achievements storage - tracks completed quests
(define-map user-achievements
  { user: principal, achievement-id: uint }
  {
    quest-id: uint,
    completed-at: uint,
    proof-url: (optional (string-ascii 255)),
    verification-count: uint,
    is-verified: bool
  }
)

;; Verification contributions - tracks who verified which achievements
(define-map verification-contributions
  { verifier: principal, achievement-id: uint }
  {
    verified-at: uint,
    comment: (optional (string-utf8 500))
  }
)

;; Maps a user's friends for privacy settings
(define-map user-friends
  { user: principal, friend: principal }
  { added-at: uint }
)

;; Contract owner for administrative functions
(define-data-var contract-owner principal tx-sender)

;; Private functions

;; Check if the provided privacy setting is valid
(define-private (is-valid-privacy-setting (setting uint))
  (or
    (is-eq setting PRIVACY-PUBLIC)
    (is-eq setting PRIVACY-FRIENDS-ONLY)
    (is-eq setting PRIVACY-PRIVATE)
  )
)

;; Check if a user exists
(define-private (user-exists (user principal))
  (is-some (map-get? user-profiles { user: user }))
)

;; Check if users are friends
(define-private (are-friends (user-a principal) (user-b principal))
  (is-some (map-get? user-friends { user: user-a, friend: user-b }))
)

;; Check if caller can view user's profile based on privacy settings
(define-private (can-view-profile (viewer principal) (profile-owner principal))
  (let ((profile (unwrap! (map-get? user-profiles { user: profile-owner }) false)))
    (or
      (is-eq viewer profile-owner)
      (is-eq (get privacy-setting profile) PRIVACY-PUBLIC)
      (and 
        (is-eq (get privacy-setting profile) PRIVACY-FRIENDS-ONLY)
        (are-friends profile-owner viewer)
      )
    )
  )
)

;; Increment user reputation score
(define-private (increment-reputation (user principal) (amount uint))
  (match (map-get? user-profiles { user: user })
    profile (begin
      (map-set user-profiles
        { user: user }
        (merge profile { reputation-score: (+ (get reputation-score profile) amount) })
      )
      true
    )
    false
  )
)

;; Read-only functions

;; Get user profile if accessible based on privacy settings
(define-read-only (get-profile (user principal))
  (let ((viewer tx-sender))
    (if (can-view-profile viewer user)
      (match (map-get? user-profiles { user: user })
        profile (ok profile)
        (err ERR-USER-NOT-FOUND)
      )
      (err ERR-NOT-AUTHORIZED)
    )
  )
)

;; Get achievement details if accessible based on privacy settings
(define-read-only (get-achievement (user principal) (achievement-id uint))
  (let ((viewer tx-sender))
    (if (can-view-profile viewer user)
      (match (map-get? user-achievements { user: user, achievement-id: achievement-id })
        achievement (ok achievement)
        (err ERR-INVALID-ACHIEVEMENT)
      )
      (err ERR-NOT-AUTHORIZED)
    )
  )
)

;; Get all achievements for a user (paginated)
(define-read-only (get-user-achievements (user principal) (offset uint) (limit uint))
  (let ((viewer tx-sender))
    (if (can-view-profile viewer user)
      ;; In practice, you would implement pagination logic here
      ;; This is simplified due to Clarity's limitations with dynamic lists
      (ok true)
      (err ERR-NOT-AUTHORIZED)
    )
  )
)

;; Check if users are friends
(define-read-only (check-friendship (user-a principal) (user-b principal))
  (ok (is-some (map-get? user-friends { user: user-a, friend: user-b })))
)

;; Public functions

;; Create a new user profile
(define-public (create-profile (display-name (string-ascii 50)) (bio (string-utf8 500)) (privacy-setting uint))
  (let ((user tx-sender))
    (asserts! (not (user-exists user)) ERR-USER-ALREADY-EXISTS)
    (asserts! (is-valid-privacy-setting privacy-setting) ERR-INVALID-PRIVACY-SETTING)
    
    (map-set user-profiles
      { user: user }
      {
        display-name: display-name,
        bio: bio,
        joined-at: block-height,
        reputation-score: u0,
        quests-completed: u0,
        quests-verified: u0,
        privacy-setting: privacy-setting
      }
    )
    (ok true)
  )
)

;; Update user profile
(define-public (update-profile (display-name (string-ascii 50)) (bio (string-utf8 500)) (privacy-setting uint))
  (let ((user tx-sender))
    (asserts! (user-exists user) ERR-USER-NOT-FOUND)
    (asserts! (is-valid-privacy-setting privacy-setting) ERR-INVALID-PRIVACY-SETTING)
    
    (match (map-get? user-profiles { user: user })
      profile (begin
        (map-set user-profiles
          { user: user }
          (merge profile {
            display-name: display-name,
            bio: bio,
            privacy-setting: privacy-setting
          })
        )
        (ok true)
      )
      (err ERR-USER-NOT-FOUND)
    )
  )
)

;; Add a new achievement (called by quest contracts when a user completes a quest)
(define-public (add-achievement (user principal) (achievement-id uint) (quest-id uint) (proof-url (optional (string-ascii 255))))
  ;; This would be restricted to specific contract principals in production
  ;; For now it checks if tx-sender is the contract owner
  (let ((caller tx-sender))
    (asserts! (or (is-eq caller (var-get contract-owner)) (is-eq caller user)) ERR-NOT-AUTHORIZED)
    (asserts! (user-exists user) ERR-USER-NOT-FOUND)
    
    ;; Add the achievement
    (map-set user-achievements
      { user: user, achievement-id: achievement-id }
      {
        quest-id: quest-id,
        completed-at: block-height,
        proof-url: proof-url,
        verification-count: u0,
        is-verified: false
      }
    )
    
    ;; Update user stats
    (match (map-get? user-profiles { user: user })
      profile (begin
        (map-set user-profiles
          { user: user }
          (merge profile { quests-completed: (+ (get quests-completed profile) u1) })
        )
        ;; Add reputation for completing quest
        (increment-reputation user u5)
        (ok true)
      )
      (err ERR-USER-NOT-FOUND)
    )
  )
)

;; Verify someone else's achievement
(define-public (verify-achievement (user principal) (achievement-id uint) (comment (optional (string-utf8 500))))
  (let ((verifier tx-sender))
    ;; Can't verify your own achievements
    (asserts! (not (is-eq verifier user)) ERR-NOT-AUTHORIZED)
    (asserts! (user-exists verifier) ERR-USER-NOT-FOUND)
    
    ;; Check that the achievement exists
    (match (map-get? user-achievements { user: user, achievement-id: achievement-id })
      achievement (begin
        ;; Record the verification
        (map-set verification-contributions
          { verifier: verifier, achievement-id: achievement-id }
          {
            verified-at: block-height,
            comment: comment
          }
        )
        
        ;; Update the achievement verification count
        (map-set user-achievements
          { user: user, achievement-id: achievement-id }
          (merge achievement {
            verification-count: (+ (get verification-count achievement) u1),
            is-verified: true
          })
        )
        
        ;; Update verifier stats
        (match (map-get? user-profiles { user: verifier })
          profile (begin
            (map-set user-profiles
              { user: verifier }
              (merge profile { quests-verified: (+ (get quests-verified profile) u1) })
            )
            ;; Add reputation for verifying
            (increment-reputation verifier u2)
            (ok true)
          )
          (err ERR-USER-NOT-FOUND)
        )
      )
      (err ERR-INVALID-ACHIEVEMENT)
    )
  )
)

;; Add a friend relationship
(define-public (add-friend (friend principal))
  (let ((user tx-sender))
    (asserts! (user-exists user) ERR-USER-NOT-FOUND)
    (asserts! (user-exists friend) ERR-USER-NOT-FOUND)
    (asserts! (not (is-eq user friend)) ERR-NOT-AUTHORIZED)
    
    (map-set user-friends
      { user: user, friend: friend }
      { added-at: block-height }
    )
    (ok true)
  )
)

;; Remove a friend relationship
(define-public (remove-friend (friend principal))
  (let ((user tx-sender))
    (asserts! (is-some (map-get? user-friends { user: user, friend: friend })) ERR-USER-NOT-FOUND)
    
    (map-delete user-friends { user: user, friend: friend })
    (ok true)
  )
)

;; Administrative function to update contract owner
(define-public (set-contract-owner (new-owner principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (var-set contract-owner new-owner)
    (ok true)
  )
)