#lang typed/racket

(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")
(require "../include/cs151-universe.rkt")

(require typed/test-engine/racket-tests)

;;---------------------Data Structures + Type Definitions----------------------

(define-struct (Some T)
  ([value : T]))

(define-type (Optional T)
  (U 'None (Some T)))

(define-type TickInterval
  Positive-Exact-Rational)

(define-struct Date
  ([month : Integer]
   [day : Integer]
   [year : Integer]))

(define-struct (Pairof A B)
  ([fst : A]
   [snd : B]))

(define-type Stroke
  (U 'Freestyle 'Backstroke 'Breaststroke 'Butterfly))

(define-struct Event
  ([gender : (U 'Men 'Women)]
   [race-distance : Integer]
   [stroke : Stroke]
   [name : String]
   [date : Date]))

(define-type Country
  (U 'AFG 'ALB 'ALG 'AND 'ANG 'ANT 'ARG 'ARM 'ARU 'ASA 'AUS 'AUT 'AZE 'BAH
     'BAN 'BAR 'BDI 'BEL 'BEN 'BER 'BHU 'BIH 'BIZ 'BLR 'BOL 'BOT 'BRA 'BRN
     'BRU 'BUL 'BUR 'CAF 'CAM 'CAN 'CAY 'CGO 'CHA 'CHI 'CHN 'CIV 'CMR 'COD
     'COK 'COL 'COM 'CPV 'CRC 'CRO 'CUB 'CYP 'CZE 'DEN 'DJI 'DMA 'DOM 'ECU
     'EGY 'ERI 'ESA 'ESP 'EST 'ETH 'FIJ 'FIN 'FRA 'FSM 'GAB 'GAM 'GBR 'GBS
     'GEO 'GEQ 'GER 'GHA 'GRE 'GRN 'GUA 'GUI 'GUM 'GUY 'HAI 'HON 'HUN 'INA
     'IND 'IRI 'IRL 'IRQ 'ISL 'ISR 'ISV 'ITA 'IVB 'JAM 'JOR 'JPN 'KAZ 'KEN
     'KGZ 'KIR 'KOR 'KOS 'KSA 'KUW 'LAO 'LAT 'LBA 'LBN 'LBR 'LCA 'LES 'LIE
     'LTU 'LUX 'MAD 'MAR 'MAS 'MAW 'MDA 'MDV 'MEX 'MGL 'MHL 'MKD 'MLI 'MLT
     'MNE 'MON 'MOZ 'MRI 'MTN 'MYA 'NAM 'NCA 'NED 'NEP 'NGR 'NIG 'NOR 'NRU
     'NZL 'OMA 'PAK 'PAN 'PAR 'PER 'PHI 'PLE 'PLW 'PNG 'POL 'POR 'PRK 'QAT
     'ROU 'RSA 'ROC 'RUS 'RWA 'SAM 'SEN 'SEY 'SGP 'SKN 'SLE 'SLO 'SMR 'SOL
     'SOM 'SRB 'SRI 'SSD 'STP 'SUD 'SUI 'SUR 'SVK 'SWE 'SWZ 'SYR 'TAN 'TGA
     'THA 'TJK 'TKM 'TLS 'TOG 'TTO 'TUN 'TUR 'TUV 'UAE 'UGA 'UKR 'URU 'USA
     'UZB 'VAN 'VEN 'VIE 'VIN 'YEM 'ZAM 'ZIM))

(define-struct IOC
  ([abbrev : Country]
   [country : String]))

(define-struct Swimmer
  ([lname : String]
   [fname : String]
   [country : Country]
   [height : Real]))

(define-struct Result
  ([swimmer : Swimmer]
   [splits : (Listof Real)]))

(define-type Mode
  (U 'choose 'running 'paused 'done))

(define-struct Sim
  ([mode : Mode]
   [event : Event]
   [tick-rate : TickInterval]
   [sim-speed : (U '1x '2x '4x '8x)]
   [sim-clock : Real]
   [pixels-per-meter : Integer]
   [pool : (Listof Result)] ;; in lane order
   [labels : Image] ;; corresponding to lane order
   [ranks : (Listof Integer)] ;; in lane order
   [end-time : Real]
   [file-chooser : (Optional FileChooser)]))

(define-struct Position
  ([x-position : Real]
   [direction : (U 'east 'west 'finished)]))

(define-struct (KeyValue K V)
  ([key : K]
   [value : V]))
         
(define-struct (Association K V)
  ([key=? : (K K -> Boolean)]
   [data : (Listof (KeyValue K V))]))

(define-struct FileChooser
  ([directory : String]
   [chooser : (Association Char String)])) ;; a map of chars #\a, #\b etc.
;; to file names

;;----------------------Image Function Shorthand Definitions-------------------

(define a/a above/align)

(define o/a overlay/align)

(define o overlay)

(define a above)

(define b beside)

(define p-i place-image)

(: font (-> Font-Family Image-Color Integer String Image))
;; apply font and color to a given string
;; Image Details: size (ppm), style ('normal), weight ('light)
(define (font font color ppm str)
  (text/font str (cast (abs ppm) Byte) color #f font 'normal 'light #f))

;;-------------------Map of Country Symbols to Country Strings-----------------

(: ioc-abbrevs (Listof IOC))
(define ioc-abbrevs
  (list (IOC 'AFG "Afghanistan")
        (IOC 'ALB "Albania")
        (IOC 'ALG "Algeria")
        (IOC 'AND "Andorra")
        (IOC 'ANG "Angola")
        (IOC 'ANT "Antigua Barbuda")
        (IOC 'ARG "Argentina")
        (IOC 'ARM "Armenia")
        (IOC 'ARU "Aruba")
        (IOC 'ASA "American Samoa")
        (IOC 'AUS "Australia")
        (IOC 'AUT "Austria")
        (IOC 'AZE "Azerbaijan")
        (IOC 'BAH "Bahamas")
        (IOC 'BAN "Bangladesh")
        (IOC 'BAR "Barbados")
        (IOC 'BDI "Burundi")
        (IOC 'BEL "Belgium")
        (IOC 'BEN "Benin")
        (IOC 'BER "Bermuda")
        (IOC 'BHU "Bhutan")
        (IOC 'BIH "Bosnia Herzegovina")
        (IOC 'BIZ "Belize")
        (IOC 'BLR "Belarus")
        (IOC 'BOL "Bolivia")
        (IOC 'BOT "Botswana")
        (IOC 'BRA "Brazil")
        (IOC 'BRN "Bahrain")
        (IOC 'BRU "Brunei")
        (IOC 'BUL "Bulgaria")
        (IOC 'BUR "Burkina Faso")
        (IOC 'CAF "Central African Republic")
        (IOC 'CAM "Cambodia")
        (IOC 'CAN "Canada")
        (IOC 'CAY "Cayman Islands")
        (IOC 'CGO "Congo Brazzaville")
        (IOC 'CHA "Chad")
        (IOC 'CHI "Chile")
        (IOC 'CHN "China")
        (IOC 'CIV "Cote dIvoire")
        (IOC 'CMR "Cameroon")
        (IOC 'COD "Congo Kinshasa")
        (IOC 'COK "Cook Islands")
        (IOC 'COL "Colombia")
        (IOC 'COM "Comoros")
        (IOC 'CPV "Cape Verde")
        (IOC 'CRC "Costa Rica")
        (IOC 'CRO "Croatia")
        (IOC 'CUB "Cuba")
        (IOC 'CYP "Cyprus")
        (IOC 'CZE "Czechia")
        (IOC 'DEN "Denmark")
        (IOC 'DJI "Djibouti")
        (IOC 'DMA "Dominica")
        (IOC 'DOM "Dominican Republic")
        (IOC 'ECU "Ecuador")
        (IOC 'EGY "Egypt")
        (IOC 'ERI "Eritrea")
        (IOC 'ESA "El Salvador")
        (IOC 'ESP "Spain")
        (IOC 'EST "Estonia")
        (IOC 'ETH "Ethiopia")
        (IOC 'FIJ "Fiji")
        (IOC 'FIN "Finland")
        (IOC 'FRA "France")
        (IOC 'FSM "Micronesia")
        (IOC 'GAB "Gabon")
        (IOC 'GAM "Gambia")
        (IOC 'GBR "United Kingdom")
        (IOC 'GBS "Guinea-Bissau")
        (IOC 'GEO "Georgia")
        (IOC 'GEQ "Equatorial Guinea")
        (IOC 'GER "Germany")
        (IOC 'GHA "Ghana")
        (IOC 'GRE "Greece")
        (IOC 'GRN "Grenada")
        (IOC 'GUA "Guatemala")
        (IOC 'GUI "Guinea")
        (IOC 'GUM "Guam")
        (IOC 'GUY "Guyana")
        (IOC 'HAI "Haiti")
        (IOC 'HON "Honduras")
        (IOC 'HUN "Hungary")
        (IOC 'INA "Indonesia")
        (IOC 'IND "India")
        (IOC 'IRI "Iran")
        (IOC 'IRL "Ireland")
        (IOC 'IRQ "Iraq")
        (IOC 'ISL "Iceland")
        (IOC 'ISR "Israel")
        (IOC 'ISV "US Virgin Islands")
        (IOC 'ITA "Italy")
        (IOC 'IVB "British Virgin Islands")
        (IOC 'JAM "Jamaica")
        (IOC 'JOR "Jordan")
        (IOC 'JPN "Japan")
        (IOC 'KAZ "Kazakhstan")
        (IOC 'KEN "Kenya")
        (IOC 'KGZ "Kyrgyzstan")
        (IOC 'KIR "Kiribati")
        (IOC 'KOR "South Korea")
        (IOC 'KOS "Kosovo")
        (IOC 'KSA "Saudi Arabia")
        (IOC 'KUW "Kuwait")
        (IOC 'LAO "Laos")
        (IOC 'LAT "Latvia")
        (IOC 'LBA "Libya")
        (IOC 'LBN "Lebanon")
        (IOC 'LBR "Liberia")
        (IOC 'LCA "St Lucia")
        (IOC 'LES "Lesotho")
        (IOC 'LIE "Liechtenstein")
        (IOC 'LTU "Lithuania")
        (IOC 'LUX "Luxembourg")
        (IOC 'MAD "Madagascar")
        (IOC 'MAR "Morocco")
        (IOC 'MAS "Malaysia")
        (IOC 'MAW "Malawi")
        (IOC 'MDA "Moldova")
        (IOC 'MDV "Maldives")
        (IOC 'MEX "Mexico")
        (IOC 'MGL "Mongolia")
        (IOC 'MHL "Marshall Islands")
        (IOC 'MKD "North Macedonia")
        (IOC 'MLI "Mali")
        (IOC 'MLT "Malta")
        (IOC 'MNE "Montenegro")
        (IOC 'MON "Monaco")
        (IOC 'MOZ "Mozambique")
        (IOC 'MRI "Mauritius")
        (IOC 'MTN "Mauritania")
        (IOC 'MYA "Myanmar Burma")
        (IOC 'NAM "Namibia")
        (IOC 'NCA "Nicaragua")
        (IOC 'NED "Netherlands")
        (IOC 'NEP "Nepal")
        (IOC 'NGR "Nigeria")
        (IOC 'NIG "Niger")
        (IOC 'NOR "Norway")
        (IOC 'NRU "Nauru")
        (IOC 'NZL "New Zealand")
        (IOC 'OMA "Oman")
        (IOC 'PAK "Pakistan")
        (IOC 'PAN "Panama")
        (IOC 'PAR "Paraguay")
        (IOC 'PER "Peru")
        (IOC 'PHI "Philippines")
        (IOC 'PLE "Palestinian Territories")
        (IOC 'PLW "Palau")
        (IOC 'PNG "Papua New Guinea")
        (IOC 'POL "Poland")
        (IOC 'POR "Portugal")
        (IOC 'PRK "North Korea")
        (IOC 'QAT "Qatar")
        (IOC 'ROU "Romania")
        (IOC 'RSA "South Africa")
        (IOC 'ROC "Russia")
        (IOC 'RUS "Russia")
        (IOC 'RWA "Rwanda")
        (IOC 'SAM "Samoa")
        (IOC 'SEN "Senegal")
        (IOC 'SEY "Seychelles")
        (IOC 'SGP "Singapore")
        (IOC 'SKN "St Kitts Nevis")
        (IOC 'SLE "Sierra Leone")
        (IOC 'SLO "Slovenia")
        (IOC 'SMR "San Marino")
        (IOC 'SOL "Solomon Islands")
        (IOC 'SOM "Somalia")
        (IOC 'SRB "Serbia")
        (IOC 'SRI "Sri Lanka")
        (IOC 'SSD "South Sudan")
        (IOC 'STP "Sao Tome Principe")
        (IOC 'SUD "Sudan")
        (IOC 'SUI "Switzerland")
        (IOC 'SUR "Suriname")
        (IOC 'SVK "Slovakia")
        (IOC 'SWE "Sweden")
        (IOC 'SWZ "Eswatini")
        (IOC 'SYR "Syria")
        (IOC 'TAN "Tanzania")
        (IOC 'TGA "Tonga")
        (IOC 'THA "Thailand")
        (IOC 'TJK "Tajikistan")
        (IOC 'TKM "Turkmenistan")
        (IOC 'TLS "Timor Leste")
        (IOC 'TOG "Togo")
        (IOC 'TTO "Trinidad Tobago")
        (IOC 'TUN "Tunisia")
        (IOC 'TUR "Turkey")
        (IOC 'TUV "Tuvalu")
        (IOC 'UAE "United Arab Emirates")
        (IOC 'UGA "Uganda")
        (IOC 'UKR "Ukraine")
        (IOC 'URU "Uruguay")
        (IOC 'USA "United States")
        (IOC 'UZB "Uzbekistan")
        (IOC 'VAN "Vanuatu")
        (IOC 'VEN "Venezuela")
        (IOC 'VIE "Vietnam")
        (IOC 'VIN "St Vincent Grenadines")
        (IOC 'YEM "Yemen")
        (IOC 'ZAM "Zambia")
        (IOC 'ZIM "Zimbabwe")))

;;-------------------------Useful Polymorphic Functions------------------------

(: quick-sort (All (A) (-> (Listof A) (-> A A Boolean) (Listof A))))
;; produce a sorted list in order according to the given comparison function
(define (quick-sort lst comp-f)
  (match lst
    ['() '()]
    [(cons hd tl)
     (local
       {(define l (filter (lambda ([x : A]) (comp-f x hd)) tl))
        (define r (filter (lambda ([x : A]) (not (comp-f x hd))) tl))}
       (append (quick-sort l comp-f) (list hd) (quick-sort r comp-f)))]))

(check-expect (quick-sort (list 8 7 6 5 4 3 2 1) <=)
              '(1 2 3 4 5 6 7 8))
(check-expect (quick-sort (list 1 2 3 3 4 5 6 6 7) >=)
              '(7 6 6 5 4 3 3 2 1))
(check-expect (quick-sort (list "a" "c" "b" "f" "e" "d" "g") string<?)
              '("a" "b" "c" "d" "e" "f" "g"))

(: opt-map (All (A B) (-> (-> A B) (Optional A) (Optional B))))
;; return 'None if 'None and apply function to a if given (Some a)
(define (opt-map f opt)
  (match opt
    ['None 'None]
    [(Some a) (Some (f a))]))

(check-expect (opt-map add1 (Some 20)) (Some 21))
(check-expect (opt-map add1 'None) 'None)

(: index-of (All (A) (-> (-> A A Boolean) A (Listof A) (Optional Integer))))
;; return the position (starting from 1) of first occurence of item
(define (index-of f item lst)
  (match lst
    ['() 'None]
    [(cons hd tl)
     (if (f item hd) (Some 1)
         (opt-map add1 (index-of f item tl)))]))

(check-expect (index-of = 86.77 '(27.89 57.54 86.77 116.39)) (Some 3))
(check-expect (index-of = 58.83 '(27.89 57.54 86.77 116.39)) 'None)

(: in-lst? : All (A) (-> A A Boolean) A (Listof A) -> Boolean)
;; check whether of not the given element is present in the list of element
(define (in-lst? f elmt lst)
  (match lst
    ['() #f]
    [(cons hd tl)
     (if (f hd elmt) #t
         (in-lst? f elmt tl))]))

(check-expect (in-lst? = 24.3 (list 23.81 24.07 24.21 24.3 24.32 24.32)) #t)
(check-expect (in-lst? = 20.2 (list 23.81 24.07 24.21 24.3 24.32 24.32)) #f)

;;--------------------------Find Association Function--------------------------

(: find-assoc : All (K V) K (Association K V) -> (Optional V))
;; given a key and an association, return the corresponding value,
;; if there is one
(define (find-assoc k assoc)
  (match assoc
    [(Association key=? data)
     (match data
       ['() 'None]
       [(cons (KeyValue key val) tl)
        (if (key=? k key)
            (Some val)
            (find-assoc k (Association key=? tl)))])]))

(check-expect (find-assoc 2 (Association = (list (KeyValue 1 "hello")
                                                 (KeyValue 2 "hey")
                                                 (KeyValue 3 "hi"))))
              (Some "hey"))
(check-expect (find-assoc 4 (Association = (list (KeyValue 1 "hello")
                                                 (KeyValue 2 "hey")
                                                 (KeyValue 3 "hi"))))
              'None)
(check-expect (find-assoc "1" (Association string=? (list (KeyValue "1" "hello")
                                                          (KeyValue "2" "hey")
                                                          (KeyValue "3" "hi"))))
              (Some "hello"))

;;---------------------------------Split Functions-----------------------------

(: splt-acc : Char (Listof Char) String -> String)
;; return a string of the given list of characters up until a given character
(define (splt-acc c chars acc)
  (match chars
    ['() acc]
    [(cons hd tl)
     (if (char=? c hd) acc (string-append (list->string (list hd))
                                          (splt-acc c tl acc)))]))

(check-expect (splt-acc #\l (list #\a #\b #\l #\c #\d #\l #\e #\f #\l) "")
              "ab")
(check-expect (splt-acc #\d (list #\a #\b #\l #\c #\d #\l #\e #\f #\l) "")
              "ablc")
(check-expect (splt-acc #\f (list #\a #\b #\l #\c #\d #\l #\e #\f #\l) "")
              "ablcdle")

(: split : Char String -> (Listof String))
;; split a string around the given character
;; ex: (split #\x "abxcdxyyz") -> (list "ab" "cd" "yyz")
;; ex: (split #\, "Chicago,IL,60637") -> (list "Chicago" "IL" "60637")
;; ex: (split #\: "abcd") -> (list "abcd")
(define (split char str)
  (match str
    ["" '()]
    [_ (if (not (in-lst? char=? char (string->list str))) (list str)
           (local
             {(define chars (string->list str))
              (define splt-str (splt-acc char chars ""))
              (define trm-str (string-trim str splt-str #:right? #f))
              (define rest-str (substring trm-str 1 (string-length trm-str)))}
             (cons splt-str (if (= (length (string->list rest-str)) 0)
                                (list "")
                                (split char rest-str)))))]))

(check-expect (split #\x "abxcdxyyz") '("ab" "cd" "yyz"))
(check-expect (split #\, "Chicago,IL,60637") '("Chicago" "IL" "60637"))
(check-expect (split #\: "abcd") '("abcd"))
(check-expect (split #\| "2|Wasick|Katarzyna|POL|1.78|24.32")
              '("2" "Wasick" "Katarzyna" "POL" "1.78" "24.32"))
(check-expect (split #\, "ab,,cd") '("ab" "" "cd"))
(check-expect (split #\, "ab,") '("ab" ""))
(check-expect (split #\, "ab,cd,ef,") '("ab" "cd" "ef" ""))

;;-----------------------------Clock Format Function---------------------------

(: mmsshh : Real -> String)
;; display an amount of time in MM:SS.HH format
;; where HH are hundredths of seconds
;; - don't worry about hours, since races are at most
;;   a few minutes long
;; - *do* append a trailing zero as needed
;; ex: (mmsshh 62.23) -> "1:02.23"
;; ex: (mmsshh 62.2)  -> "1:02.20"
(define (mmsshh time)
  (local
    {(define t (real->decimal-string time 2))
     (define min (floor (/ time 60)))
     (define sec (real->decimal-string (- time (* min 60)) 2))}
    (if (< time 60) t
        (local
          {(define minutes (real->decimal-string min 0))}
          (string-append (if (= (string-length minutes) 2)
                             (substring minutes 0 1)
                             (substring minutes 0 2))
                         ":"
                         (if (= (string-length sec) 4)
                             (string-append "0" sec)
                             sec))))))

(check-expect (mmsshh 62.23) "1:02.23")
(check-expect (mmsshh 62.2) "1:02.20")
(check-expect (mmsshh 1622.35) "27:02.35")
(check-expect (mmsshh 60.0) "1:00.00")
(check-expect (mmsshh 0) "0.00")
(check-expect (mmsshh 2.3) "2.30")

;;-------------------------------Flag Functions--------------------------------

(: country-sym->str : Country (Listof IOC) -> String)
;; return the given country's corresponding country string
;; given a country and a map of countries to country strings 
(define (country-sym->str cntry ioc-lst)
  (match ioc-lst
    [(cons (IOC sym str) tl)
     (if (symbol=? cntry sym) str
         (country-sym->str cntry tl))]))

(: flag-of : Country -> Image)
;; produce an image of a country's flag
;; - use bitmap/file and find the file include/flags
;; - it is OK to raise an error for a not-found file
(define (flag-of cntry)
  (local
    {(define c-str (string-downcase (country-sym->str cntry ioc-abbrevs)))}
    (bitmap/file (string-append "../include/flags/"
                                (string-replace c-str " " "-")
                                ".png"))))

;;--------------------------Current Position Functions-------------------------

(: last-item : (Listof Real) ->  Real)
;; return the last item of a list of reals
;; helper function for end-times
;; note that base case only works if used as this specific helper function
(define (last-item lst)
  (match lst
    ['() 0]
    [(cons hd '()) hd]
    [(cons hd tl) (last-item tl)]))

(: end-times : Result -> (Listof Real))
;; produce a list of times where each time is the overall time at the
;; completion of each split
(define (end-times result)
  (local
    {(define splits (Result-splits result))
     (: times-acc : (Listof Real) (Listof Real) -> (Listof Real))
     ;; accumulate a list of end-times at the completion of each split
     (define (times-acc splts acc)
       (match splts
         ['() acc]
         [(cons hd tl)
          (times-acc tl (append acc (list (+ hd (last-item acc)))))]))}
    (times-acc splits '())))

(: curr-splt : Real (Listof Real) -> Real)
;; return the current end-split time given the current race time and a list of
;; overall times at the completion of each split (end-times)
(define (curr-splt time end-times)
  (match end-times
    ['() (error "empty list")]
    [(cons hd tl)
     (if (<= time hd) hd (curr-splt time tl))]))

(: retrieve-int (-> (Optional Integer) Integer))
;; retrieve the integer, given an optional integer (if 'None raise error)
(define (retrieve-int opt-int)
  (match opt-int
    ['None (error "optional is 'None")]
    [(Some int) int]))

(: swmr-time : Result -> Real)
;; return the swimmer's final time given their result
(define (swmr-time result)
  (foldr + 0 (Result-splits result)))

(: current-position : Real Result -> Position)
;; the arguments to current-position are the current time and a result
;; - compute the given swimmer's current position, which
;;   includes a heading 'east or 'west, or 'finished
(define (current-position t result)
  (local
    {(define h (Swimmer-height (Result-swimmer result)))}
    (if (>= t (swmr-time result)) (Position (- 50 (/ h 2)) 'finished)
        (local
          {(define splts (Result-splits result))
           (define end-splts (end-times result))
           (define splt-posn (retrieve-int (index-of = (curr-splt t end-splts)
                                                     end-splts)))
           (define splt (list-ref splts (- splt-posn 1)))
           (define time (if (<= t (first splts)) t
                            (- t (list-ref end-splts (- splt-posn 2)))))
           (define dist (+ (/ h 2) (* time (/ (- 50 h) splt))))}
          (if (= (length splts) 1)
              (Position dist 'east)
              (if (odd? splt-posn)
                  (Position (- 50 dist) 'west)
                  (Position dist 'east)))))))

;;-------------------------Compute-Ranks Functions-----------------------------

(: times : (Listof Result) -> (Listof Real))
;; produce a list of final times given a list of results
(define (times rslts)
  (match rslts
    ['() '()]
    [(cons rslt tl)
     (cons (swmr-time rslt) (times tl))]))

(: srt-times : (Listof Real) -> (Listof Real))
;; produce a list of times sorted from least to greatest given a list of results
(define (srt-times times)
  (quick-sort times <=))

(: rnk-lst : (Listof Real) (Listof Real) -> (Listof Integer))
;; produce a list of ranks given two sorted lists of final times
(define (rnk-lst times1 times2)
  (match times2
    ['() '()]
    [(cons hd tl)
     (local
       {(define posn (retrieve-int (index-of = hd times1)))}
       (cons posn (if (in-lst? = hd tl)
                      (append (list posn) (rnk-lst times1 (rest tl)))
                      (rnk-lst times1 tl))))]))

(: compute-ranks : (Listof Result) -> (Listof Integer))
;; produce a list of ranks given a list of results
(define (compute-ranks rslts)
  (local
    {(define ts (times rslts))
     (define srtd-ts (srt-times ts))
     (define ranks (rnk-lst srtd-ts srtd-ts))}
    (map (lambda ([x : Real])
           (list-ref ranks (- (retrieve-int (index-of = x srtd-ts)) 1))) ts)))

;---------------------------Compute-Labels Function----------------------------

(: compute-lbls : (Listof Result) Integer -> Image)
;; produce the labels for the swimmers in the race given the list of results
(define (compute-lbls results ppm)
  (match results
    ['() empty-image]
    [(cons (Result swmr _) tl)
     (local
       {(define lname (Swimmer-lname swmr))
        (define fname (Swimmer-fname swmr))
        (define cntry (Swimmer-country swmr))
        (define name (string-append " " (substring fname 0 1) ". " lname))}
       (a/a 'left
            (o (b (scale (* ppm 0.018) (flag-of cntry))
                  (font 'modern 'black ppm name))
               (rectangle ppm (* ppm 2.5) 'solid 'white))
            (compute-lbls tl ppm)))]))

;;-------------------------Compute-End-Time Function---------------------------

(: compute-time : (Listof Result) -> Real)
;; produce the end-time of the simulation given the list of results
(define (compute-time results)
  (match results
    ['() 0]
    [(cons hd tl)
     (local
       {(define time (swmr-time hd))
        (define max-time (compute-time tl))}
       (if (> time max-time)
           time max-time))]))

;;---------------------------Initial-Sim Function------------------------------

(: initial-sim : Event TickInterval Integer (Listof Result) -> Sim)
;; the parameters are an event, a tick interval, pixels per meter,
;; and a list of results corresponding to a race
;; - this function must precompute the labels, ranks, and end-time
;;   values based on the list of results
(define (initial-sim ev t-rate ppm pl)
  (Sim 'running ev t-rate '1x 0 ppm pl (compute-lbls pl ppm)
       (compute-ranks pl) (compute-time pl) 'None))

;;----------------------------Sim-From-File Functions--------------------------

(: find-gender : (Listof String) -> (U 'Men 'Women))
;; produce a gender given the lines of a swm file
(define (find-gender lines)
  (match lines
    ['() (error "not in file")]
    [(cons hd tl)
     (if (string-prefix? hd "gender:")
         (match (string-trim hd "gender:")
           ["w" 'Women]
           ["m" 'Men])
         (find-gender tl))]))

(: find-rc-dist : (Listof String) -> Integer)
;; produce a race-distance given the lines of a swm file
(define (find-rc-dist lines)
  (match lines
    ['() (error "not in file")]
    [(cons hd tl)
     (if (string-prefix? hd "distance:")
         (cast (string->number (string-trim hd "distance:")) Integer)
         (find-rc-dist tl))]))
                  
(: find-stroke : (Listof String) -> Stroke)
;; produce a stroke given the lines of a swm file
(define (find-stroke lines)
  (match lines
    ['() (error "not in file")]
    [(cons hd tl)
     (if (string-prefix? hd "stroke:")
         (cast (string->symbol (string-trim hd "stroke:")) Stroke)
         (find-stroke tl))]))

(: find-event : (Listof String) -> String)
;; produce an event-name given the lines of a swm file
(define (find-event lines)
  (match lines
    ['() (error "not in file")]
    [(cons hd tl)
     (if (string-prefix? hd "event:")
         (string-trim hd "event:")
         (find-event tl))]))

(: lst->date : (Listof String) -> Date)
;; produce a date given a list of strings of month, day, and year
(define (lst->date mdy)
  (Date (cast (string->number (list-ref mdy 0)) Integer)
        (cast (string->number (list-ref mdy 1)) Integer)
        (cast (string->number (list-ref mdy 2)) Integer)))

(: find-date : (Listof String) -> Date)
;; produce a data given the lines of a swm file
(define (find-date lines)
  (match lines
    ['() (error "not in file")]
    [(cons hd tl)
     (if (string-prefix? hd "date:")
         (lst->date (split #\| (string-trim hd "date:")))
         (find-date tl))]))

(: ev-from-file : String -> Event)
;; produce an event given a swm file
(define (ev-from-file file)
  (local
    {(define lines (file->lines file))}
    (Event (find-gender lines)
           (find-rc-dist lines)
           (find-stroke lines)
           (find-event lines)
           (find-date lines))))

(: find-swmrs : (Listof String) -> (Listof (KeyValue Integer Swimmer)))
;; produce a list of ranks and swimmers given the lines of a swm file
(define (find-swmrs lines)
  (match lines
    ['() '()]
    [(cons hd tl)
     (if (string-prefix? hd "result:")
         (local
           {(define rslt (split #\| (string-trim hd "result:")))}
           (cons (KeyValue (cast (string->number (list-ref rslt 0)) Integer)
                           (Swimmer (list-ref rslt 1)
                                    (list-ref rslt 2)
                                    (cast (string->symbol (list-ref rslt 3))
                                          Country)
                                    (cast (string->number (list-ref rslt 4))
                                          Real)))
                 (find-swmrs tl)))
         (find-swmrs tl))]))

(: rank-s<? : (KeyValue Integer Swimmer) (KeyValue Integer Swimmer) -> Boolean)
;; check whether of not the first key-value pair's rank is less than the second
;; when given two key-value pairs of rank and a swimmer
(define (rank-s<? kv1 kv2)
  (match* (kv1 kv2)
    [((KeyValue rnk1 _) (KeyValue rnk2 _)) (< rnk1 rnk2)]))

(: srt-swmrs : (Listof (KeyValue Integer Swimmer)) -> (Listof Swimmer))
;; sort the list of key-value pairs by rank and return the swimmers
(define (srt-swmrs k-vs)
  (map (lambda ([x : (KeyValue Integer Swimmer)])
         (match x
           [(KeyValue rnk swmr) swmr]))
       (quick-sort k-vs rank-s<?)))

(: find-splts : (Listof String) -> (Listof (KeyValue Integer (Listof Real))))
;; produce a list of ranks and swimmer's splits given the lines of a swm file
(define (find-splts lines)
  (match lines
    ['() '()]
    [(cons hd tl)
     (if (string-prefix? hd "result:")
         (cons (local
                 {(define rslt (split #\| (string-trim hd "result:")))
                  (define splts (split #\, (list-ref rslt 5)))
                  (: splts->reals : (Listof String) -> (Listof Real))
                  (define (splts->reals splits)
                    (match splits
                      ['() '()]
                      [(cons hd tl) (cons (cast (string->number hd) Real)
                                          (splts->reals tl))]))}
                 (KeyValue (cast (string->number (list-ref rslt 0)) Integer)
                           (splts->reals splts)))
               (find-splts tl))
         (find-splts tl))]))

(: rank-sp<? : (KeyValue Integer (Listof Real)) (KeyValue Integer (Listof Real))
   -> Boolean)
;; check whether of not the first key-value pair's rank is less than the second
;; when given two key value pairs of rank and splits
(define (rank-sp<? kv1 kv2)
  (match* (kv1 kv2)
    [((KeyValue rnk1 _) (KeyValue rnk2 _)) (< rnk1 rnk2)]))

(: srt-splts : (Listof (KeyValue Integer (Listof Real)))
   -> (Listof (Listof Real)))
;; sort the list of key-value pairs by rank and return the splits
(define (srt-splts k-vs)
  (map (lambda ([x : (KeyValue Integer (Listof Real))])
         (match x
           [(KeyValue rnk splts) splts]))
       (quick-sort k-vs rank-sp<?)))

(: find-pl : (Listof Swimmer) (Listof (Listof Real)) -> (Listof Result))
;; produce a list of results given the lines of a swm file
(define (find-pl swmrs splts-lst)
  (match* (swmrs splts-lst)
    [('() '()) '()]
    [((cons swmr swmrs-tl) (cons splts splts-tl))
     (cons (Result swmr splts)
           (find-pl swmrs-tl splts-tl))]))

(: sim-from-file : TickInterval Integer String -> Sim)
;; given a tick interval, a pixels-per-meter, and the name of an swm file,
;; build a Sim that contains the data from the file
;; - note: the Sim constructed by this function should contain 'None
;;         in the file-chooser slot
;; - note: GIGO applies to this function in all ways
(define (sim-from-file t-rate ppm file)
  (local
    {(define lines (file->lines file))
     (define ev (ev-from-file file))
     (define pl (find-pl (srt-swmrs (find-swmrs lines))
                         (srt-splts (find-splts lines))))}
    (initial-sim ev t-rate ppm pl)))

;;------------------------Build-File-Chooser Functions-------------------------

(: suff-paths : String (Listof String) -> (Listof String))
;; filter the list of paths, returning only those which have the given suffix
(define (suff-paths suff paths)
  (filter (lambda ([x : String]) (string-suffix? x suff)) paths))

(: alph : (Listof Char))
(define alph (list #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n
                   #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z))

(: build-file-chooser : String String -> FileChooser)
;; given a suffix and a directory name, build a file chooser
;; associating the characters a, b, c, etc. with all the files
;; in the given directory that have the given suffix
;; - note: you don't need to support more than 26 files
;;         (which would exhaust the alphabet) -- consider that
;;         GIGO if it happens
(define (build-file-chooser suff dir)
  (local
    {(define paths (suff-paths suff (map path->string (directory-list dir))))
     (: paths->k-vs : (Listof Char) (Listof String)
        -> (Listof (KeyValue Char String)))
     ;; produce an accumulated list of key-value pairs
     ;; where keys are alphabetic characters and values are paths
     (define (paths->k-vs alph paths)
       (match* (alph paths)
         [('() _) '()]
         [(_ '()) '()]
         [((cons h1 t1) (cons h2 t2))
          (cons (KeyValue h1 h2) (paths->k-vs t1 t2))]))}
    (FileChooser dir (Association char=? (paths->k-vs alph paths)))))

;;----------------------Directory ('choose mode) Functions---------------------

(: initial-dir : TickInterval Integer String -> Sim)
;; produce a sim of mode 'choose with constructed file-chooser
;; given a tick interval, pixels per meter, and a directory path
;; note that dummy values are added for unrelated parameters of sim
(define (initial-dir t-rate ppm dir)
  (Sim 'choose (Event 'Men 0 'Freestyle "" (Date 0 0 0)) t-rate '1x 0 ppm '()
       empty-image '() 0 (Some (build-file-chooser ".swm" dir))))

(: revisit-dir : TickInterval Integer (Optional FileChooser) -> Sim)
;; produce a sim of mode 'choose with precomputed file-chooser
;; this function is specifically for revisiting the directory when d is pressed
;; note that dummy values are added for unrelated parameters of sim
(define (revisit-dir t-rate ppm fc)
  (Sim 'choose (Event 'Men 0 'Freestyle "" (Date 0 0 0)) t-rate '1x 0 ppm '()
       empty-image '() 0 fc))

;-------------------React-to Key, Mouse, and Tick Functions--------------------
  
(: set-mode : Mode Sim -> Sim)
;; set the mode in simulation
(define (set-mode mode sim)
  (match sim
    [(Sim _ ev t-rate spd clck ppm pl lbls ranks t fc)
     (Sim mode ev t-rate spd clck ppm pl lbls ranks t fc)]))

(: set-speed : (U '1x '2x '4x '8x) Sim -> Sim)
;; set the simulation speed
(define (set-speed s sim)
  (match sim
    [(Sim m ev t-rate _ clck ppm pl lbls ranks t fc)
     (Sim m ev t-rate s clck ppm pl lbls ranks t fc)]))

(: toggle-paused : Sim -> Sim)
;; set 'running sim to 'paused, and set 'paused sim to 'running
;; return 'done sim as is
(define (toggle-paused sim)
  (match (Sim-mode sim)
    ['running (set-mode 'paused sim)]
    ['paused (set-mode 'running sim)]
    ['done sim]
    ['choose sim]))

(: reset : Sim -> Sim)
;; reset the simulation to the beginning of the race
(define (reset sim)
  (match sim
    [(Sim m ev t-rate spd clck ppm pl lbls ranks t fc)
     (Sim m ev t-rate spd 0 ppm pl lbls ranks t fc)]))

(: react-to-keyboard : Sim String -> Sim)
;; create sim if in 'choose mode based on key's associated value
;; set sim-speed to 1x, 2x, 4x, or 8x on "1", "2", "4", "8"
;; reset the simulation on "r"
;; return to the directory (file selector) screen on "d"
(define (react-to-keyboard sim key)
  (match sim
    [(Sim m ev t-rate spd clck ppm pl lbls ranks t fc)
     (if (symbol=? m 'choose)
         (match fc
           ['None sim]
           [(Some (FileChooser dir chooser))
            (match (find-assoc (list-ref (string->list key) 0) chooser)
              ['None sim]
              [(Some p) 
               (match (sim-from-file t-rate ppm (string-append dir "/" p))
                 [(Sim m ev t-rate spd clck ppm pl lbls ranks t _)
                  (Sim m ev t-rate spd clck ppm pl lbls ranks t fc)])])])
         (match key
           ["1" (set-speed '1x sim)]
           ["2" (set-speed '2x sim)]
           ["4" (set-speed '4x sim)]
           ["8" (set-speed '8x sim)]
           ["r" (if (symbol=? m 'done)
                    (Sim 'running ev t-rate spd 0 ppm pl lbls ranks t fc)
                    (reset sim))]
           ["d" (revisit-dir t-rate ppm fc)]
           [_ sim]))]))

(: react-to-tick : Sim -> Sim)
;; if simulation is 'running, increase sim-clock accordingly
;; note that new-time definition is from Project 1 reference implementation
(define (react-to-tick sim)
  (match sim
    [(Sim m ev t-rate spd clck ppm pl lbls ranks t fc)
     (match m
       ['done sim]
       ['paused sim]
       ['choose sim]
       ['running
        (local
          {(define new-time
             (match spd
               ['1x (+ clck t-rate)]
               ['2x (+ clck (* 2 t-rate))]
               ['4x (+ clck (* 4 t-rate))]
               ['8x (+ clck (* 8 t-rate))]))}
          (if (>= new-time t)
              (set-mode 'done sim)
              (Sim 'running ev t-rate spd new-time
                   ppm pl lbls ranks t fc)))])]))

(: react-to-mouse : Sim Integer Integer Mouse-Event -> Sim)
;; pause/unpause the simulation on "button-down"
(define (react-to-mouse sim x y event)
  (match event
    ["button-down" (toggle-paused sim)]
    [_ sim]))

;;-------------------------------Draw Functions--------------------------------

(: draw-pool : Integer -> Image)
;; draw a singular pool
(define (draw-pool ppm)
  (local
    {(define line-in-pool (rectangle (* ppm 47) (* ppm 0.2) 'solid
                                     (color 77 130 243)))}
    (frame (o (o/a 'left 'middle
                   (rectangle (* ppm 0.2) ppm 'solid (color 77 130 243)) 
                   line-in-pool)
              (o/a 'right 'middle
                   (rectangle (* ppm 0.2) ppm 'solid (color 77 130 243))
                   line-in-pool)
              (rectangle (* ppm 50) (* ppm 2.5) 'solid (color 173 217 230))))))

(: draw-arrow : Integer Position -> Image)
;; draw an arrow pointing in the correct direction given a swimmer's position
(define (draw-arrow ppm posn)
  (local
    {(define dir (Position-direction posn))
     (define s (* ppm 0.5))
     (define clr (color 234 246 63))}
    (cond
      [(symbol=? dir 'east) (rotate 90 (triangle/sss s s s 'solid clr))]
      [(symbol=? dir 'west) (rotate -90 (triangle/sss s s s 'solid clr))]
      [else empty-image])))

(: draw-rank : Integer Integer -> Image)
;; given a rank, draw a circle of corresponding color and number
(define (draw-rank ppm rank)
  (match rank
    [1 (o (font 'modern 'black ppm "1") (circle ppm 'solid 'gold))]
    [2 (o (font 'modern 'black ppm "2") (circle ppm 'solid 'silver))]
    [3 (o (font 'modern 'black ppm "3") (circle ppm 'solid (color 176 141 87)))]
    [_ (o (font 'modern 'white ppm (number->string rank))
          (circle ppm 'solid (color 12 30 148)))]))

(: draw-pools : Sim -> Image)
;; draw as many pools as there are swimmers (+ two extras on the outsides)
;; as well as the swimmers in their respective pools
;; this function also draws swimmer final times and ranks in pool if 'done
(define (draw-pools sim)
  (match sim
    [(Sim m _ _ _ clck ppm pl _ ranks _ _)
     (local
       {(: pools : (Listof Result) (Listof Integer) -> Image)
        ;; draw the pools and their swimmers as an image
        (define (pools pl ranks)
          (match* (pl ranks)
            [('() '()) empty-image]
            [((cons rslt rslt-tl) (cons hd tl))
             (local
               {(define swmr (Result-swimmer rslt))
                (define curr-posn (current-position clck rslt))}
               (a (p-i (if (symbol=? m 'done) empty-image
                           (o (draw-arrow ppm curr-posn)
                              (rectangle (* ppm (Swimmer-height swmr))
                                         (* ppm 0.6) 'solid
                                         (color 247 110 5))))
                       (* ppm (Position-x-position curr-posn)) (* ppm 1.25)
                       (if (symbol=? m 'done)
                           (o/a 'right 'middle
                                (o (font 'modern 'black ppm
                                         (mmsshh (swmr-time rslt)))
                                   (rectangle (* ppm 5) (* ppm 1.5)
                                              'solid 'white))
                                (o (draw-rank ppm hd)
                                   (draw-pool ppm)))
                           (draw-pool ppm)))
                  (pools rslt-tl tl)))]))}
       (a (draw-pool ppm)
          (pools pl ranks)
          (draw-pool ppm)))]))

(: draw-pause-overlay : Sim -> Image)
;; overlay "Paused" if 'paused
;; Note: no overlay should appear if simulation is 'running or 'done
(define (draw-pause-overlay sim)
  (match sim
    [(Sim m _ _ _ _ ppm _ _ _ _ _)
     (if (symbol=? m 'paused)
         (o (font 'modern 'black ppm "Paused")
            (rectangle (* ppm 6) (* ppm 2) 'solid (color 22 250 242)))
         empty-image)]))

(: name-event : Event -> String)
;; produce the name of the given event as a string
(define (name-event ev)
  (local
    {(define gend (string-append (symbol->string (Event-gender ev)) "'s "))
     (define dist (number->string (Event-race-distance ev)))
     (define stroke (symbol->string (Event-stroke ev)))
     (define name (Event-name ev))
     (define y (number->string (Date-year (Event-date ev))))
     (define m (number->string (Date-month (Event-date ev))))
     (define d (number->string (Date-day (Event-date ev))))}
    (string-append name ": " gend dist "m " stroke " (" m "/" d "/" y ")")))

(: draw-info : Sim -> Image)
;; draw accompanying info including current race, time, speed of simulation,
;; and options for resetting race and returning to directory
(define (draw-info sim)
  (match sim
    [(Sim m ev _ spd clck ppm _ _ _ t _)
     (a/a 'left
          (font 'modern 'black ppm (name-event ev))
          (font 'modern 'black ppm
                (if (symbol=? m 'done) (string-append "Time Elapsed: "
                                                      (mmsshh t))
                    (string-append "Current Time: " (mmsshh clck))))
          (font 'modern 'black ppm
                (if (symbol=? m 'done)
                    "Press d to return to directory and r to reset race..."
                    (string-append "Current Speed of Simulation: "
                                   (symbol->string spd))))
          (font 'modern 'black ppm (if (symbol=? m 'done) ""
                                       "Click Mouse to Pause"))
          (font 'modern 'black ppm
                (if (symbol=? m 'done) ""
                    "Press d to return to directory and r to reset...")))]))

(: retrieve-k-vs : (Optional FileChooser) -> (Listof (KeyValue Char String)))
;; retrieve the key-values from a filechooser if there is one given
(define (retrieve-k-vs opt)
  (match opt
    ['None (error "No FileChooser Given")]
    [(Some fc)
     (match fc
       [(FileChooser _ (Association _ k-vs)) k-vs])]))

(: draw-paths : Sim -> Image)
;; draw the possible paths which appear if in 'choose mode
(define (draw-paths sim)
  (match sim
    [(Sim _ _ _ _ _ ppm _ _ _ _ fc)
     (local
       {(define k-vs (retrieve-k-vs fc))
        (: paths : (Listof (KeyValue Char String)) -> Image)
        ;; draw the possible paths (and associated chars) as images
        (define (paths lst)
          (match lst
            ['() empty-image]
            [(cons (KeyValue char p) tl)
             (local
               {(define len (string-length p))}
               (a/a 'left
                    (square (/ ppm 2) 'solid (color 0 0 0 0))
                    (o/a 'left 'middle
                         (b (square ppm 'solid (color 0 0 0 0))
                            (o (font 'modern 'black ppm (~a char))
                               (square (* ppm 1.25)
                                       'solid 'white))
                            (font 'modern 'black ppm (string-append "  " p)))
                         (rectangle (* ppm (+ 2 len))
                                    (* ppm 1.5)
                                    'solid 'orange))
                    (paths tl)))]))}
       (paths k-vs))]))

(: draw-choose : Sim -> Image)
;; draw the directory and paths screen which appears if in 'choose mode
(define (draw-choose sim)
  (match sim
    [(Sim _ _ _ _ _ ppm _ _ _ _ fc)
     (match fc
       ['None empty-image]
       [(Some fc)
        (a/a 'left
             (font 'modern 'black ppm
                   (string-append "Current Directory: "
                                  (FileChooser-directory fc)))
             (square ppm 'solid (color 0 0 0 0))
             (font 'modern 'black ppm "Select Key to Run Simulation:")
             (draw-paths sim))])]))

(: draw-simulation : Sim -> Image)
;; draw the simulation in its current state
;; including both graphical and textual elements
(define (draw-simulation sim)
  (match sim
    [(Sim m _ _ _ _ ppm _ _ _ _ _)
     (if (symbol=? m 'choose)
         (o/a 'left 'top
              (draw-choose sim)
              (rectangle (* ppm 65) (* ppm 40)
                         'solid 'white))
         (o (draw-pause-overlay sim)
            (a (b (draw-pools sim)
                  (Sim-labels sim))
               (draw-info sim))))]))

;;---------------------------------Run Function--------------------------------

(: run : TickInterval Integer String -> Sim)
;; the run function should consume a tick interval, a pixels per meter,
;; and a path to a directory containing one or more .swm files
(define (run t-rate ppm dir)
  (big-bang (initial-dir t-rate ppm dir) : Sim
    [to-draw draw-simulation]
    [on-key react-to-keyboard]
    [on-mouse react-to-mouse]
    [on-tick react-to-tick t-rate]
    [name "Swimming Simulation"]))

(test)