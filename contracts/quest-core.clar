;; quest-core
;; A central contract for managing the creation, participation, and completion of personal development quests.
;; This contract allows users to create quests with specific goals and rewards, join quests they're interested in,
;; and submit evidence of completed quest objectives.

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-QUEST-NOT-FOUND (err u101))
(define-constant ERR-INVALID-DATES (err u102))
(define-constant ERR-MAX-PARTICIPANTS-REACHED (err u103))
(define-constant ERR-ALREADY-JOINED (err u104))
(define-constant ERR-NOT-PARTICIPANT (err u105))
(define-constant ERR-QUEST-EXPIRED (err u106))
(define-constant ERR-QUEST-NOT-STARTED (err u107))
(define-constant ERR-ALREADY-COMPLETED (err u108))
(define-constant ERR-INVALID-PARAMETERS (err u109))

;; Data maps and variables

;; Counter for quest IDs
(define-data-var quest-id-counter uint u0)

;; Main quest data structure
(define-map quests uint {
    creator: principal,
    title: (string-ascii 100),
    description: (string-utf8 500),
    start-block-height: uint,
    end-block-height: uint,
    max-participants: uint,
    participant-count: uint,
    reward-type: (string-ascii 20),
    reward-amount: uint,
    active: bool
})

;; Track quest participants
(define-map quest-participants { quest-id: uint, participant: principal } bool)

;; Track completed quests by users
(define-map quest-completions { quest-id: uint, participant: principal } {
    completed: bool,
    block-height: uint,
    evidence-url: (optional (string-utf8 200))
})

;; Private functions

;; Get and increment the quest counter
(define-private (get-and-increment-quest-id)
    (let ((current-id (var-get quest-id-counter)))
        (var-set quest-id-counter (+ current-id u1))
        current-id
    )
)

;; Check if a user is a participant in a quest
(define-private (is-participant (quest-id uint) (user principal))
    (default-to false (map-get? quest-participants { quest-id: quest-id, participant: user }))
)

;; Check if the quest exists and is active
(define-private (is-active-quest (quest-id uint))
    (match (map-get? quests quest-id)
        quest (get active quest)
        false
    )
)

;; Check if the quest is within its valid timeframe (started but not expired)
(define-private (is-quest-active-timeframe (quest-id uint))
    (match (map-get? quests quest-id)
        quest (and 
                (>= block-height (get start-block-height quest))
                (<= block-height (get end-block-height quest))
              )
        false
    )
)

;; Read-only functions

;; Get quest details
(define-read-only (get-quest (quest-id uint))
    (map-get? quests quest-id)
)

;; Check if a user has joined a quest
(define-read-only (has-joined-quest (quest-id uint) (user principal))
    (is-participant quest-id user)
)

;; Check if a user has completed a quest
(define-read-only (has-completed-quest (quest-id uint) (user principal))
    (match (map-get? quest-completions { quest-id: quest-id, participant: user })
        completion (get completed completion)
        false
    )
)

;; Get completion details
(define-read-only (get-completion-details (quest-id uint) (user principal))
    (map-get? quest-completions { quest-id: quest-id, participant: user })
)

;; Public functions

;; Create a new quest
(define-public (create-quest 
                (title (string-ascii 100)) 
                (description (string-utf8 500))
                (start-block-height uint) 
                (end-block-height uint)
                (max-participants uint)
                (reward-type (string-ascii 20))
                (reward-amount uint))
    (let ((new-quest-id (get-and-increment-quest-id)))
        ;; Validate parameters
        (asserts! (< start-block-height end-block-height) ERR-INVALID-DATES)
        (asserts! (>= start-block-height block-height) ERR-INVALID-DATES)
        (asserts! (> max-participants u0) ERR-INVALID-PARAMETERS)
        
        ;; Store the quest
        (map-set quests new-quest-id {
            creator: tx-sender,
            title: title,
            description: description,
            start-block-height: start-block-height,
            end-block-height: end-block-height,
            max-participants: max-participants,
            participant-count: u0,
            reward-type: reward-type,
            reward-amount: reward-amount,
            active: true
        })
        
        (ok new-quest-id)
    )
)

;; Join a quest
(define-public (join-quest (quest-id uint))
    (let ((quest (unwrap! (map-get? quests quest-id) ERR-QUEST-NOT-FOUND)))
        
        ;; Validate quest state
        (asserts! (get active quest) ERR-QUEST-NOT-FOUND)
        (asserts! (<= block-height (get end-block-height quest)) ERR-QUEST-EXPIRED)
        
        ;; Check if already joined
        (asserts! (not (is-participant quest-id tx-sender)) ERR-ALREADY-JOINED)
        
        ;; Check if max participants reached
        (asserts! (< (get participant-count quest) (get max-participants quest)) ERR-MAX-PARTICIPANTS-REACHED)
        
        ;; Update quest participation
        (map-set quest-participants { quest-id: quest-id, participant: tx-sender } true)
        
        ;; Increment participant count
        (map-set quests quest-id 
            (merge quest { participant-count: (+ (get participant-count quest) u1) })
        )
        
        (ok true)
    )
)

;; Submit quest completion with evidence
(define-public (complete-quest (quest-id uint) (evidence-url (optional (string-utf8 200))))
    (let ((quest (unwrap! (map-get? quests quest-id) ERR-QUEST-NOT-FOUND)))
        
        ;; Validate quest state and participation
        (asserts! (get active quest) ERR-QUEST-NOT-FOUND)
        (asserts! (is-participant quest-id tx-sender) ERR-NOT-PARTICIPANT)
        (asserts! (is-quest-active-timeframe quest-id) ERR-QUEST-NOT-STARTED)
        
        ;; Check if already completed
        (asserts! (not (has-completed-quest quest-id tx-sender)) ERR-ALREADY-COMPLETED)
        
        ;; Record completion
        (map-set quest-completions 
            { quest-id: quest-id, participant: tx-sender } 
            { 
                completed: true, 
                block-height: block-height, 
                evidence-url: evidence-url 
            }
        )
        
        (ok true)
    )
)

;; Update quest status (enable/disable)
(define-public (set-quest-status (quest-id uint) (active bool))
    (let ((quest (unwrap! (map-get? quests quest-id) ERR-QUEST-NOT-FOUND)))
        
        ;; Only the creator can update status
        (asserts! (is-eq tx-sender (get creator quest)) ERR-NOT-AUTHORIZED)
        
        ;; Update quest status
        (map-set quests quest-id (merge quest { active: active }))
        
        (ok true)
    )
)

;; Extend quest deadline
(define-public (extend-quest-deadline (quest-id uint) (new-end-block-height uint))
    (let ((quest (unwrap! (map-get? quests quest-id) ERR-QUEST-NOT-FOUND)))
        
        ;; Only the creator can extend deadline
        (asserts! (is-eq tx-sender (get creator quest)) ERR-NOT-AUTHORIZED)
        
        ;; Validate new end date
        (asserts! (> new-end-block-height block-height) ERR-INVALID-DATES)
        (asserts! (> new-end-block-height (get end-block-height quest)) ERR-INVALID-DATES)
        
        ;; Update quest end date
        (map-set quests quest-id (merge quest { end-block-height: new-end-block-height }))
        
        (ok true)
    )
)