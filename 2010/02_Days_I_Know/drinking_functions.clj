(import '(java.awt GridLayout)
  '(java.awt.event ActionListener)
  '(java.awt.image BufferedImage)
  '(java.io File)
  '(javax.imageio ImageIO)
  '(javax.swing JFrame JPanel JLabel JTextField JButton))

(def PANEL-WIDTH 532)
(def PANEL-HEIGHT 420)
(def SPEECH-BALLON-X 230)
(def SPEECH-BALLON-Y 70)
(def CALENDAR-LEFT-X 0)
(def CALENDAR-RIGHT-X 532)
(def CALENDAR-TOP-Y 292)
(def CALENDAR-BOTTOM-Y 400)
(def CALENDAR-DAY-WIDTH 76)

(defn create-panel [sorted-drinking-days]
  (proxy [JPanel] []
    (paintComponent [g]
      (let [all-days (list 'M 'T 'W 'T 'F 'S 'S)
            drinking-days-set (set sorted-drinking-days)
            beer-image (ImageIO/read (new File "beer.png"))]

        (.drawImage g (ImageIO/read (new File "chuck.png")) 0 0 nil)
        (.drawString g "Chuck Norris needs a drink!" SPEECH-BALLON-X SPEECH-BALLON-Y)
        (.drawString g "You better run," SPEECH-BALLON-X (+ 15 SPEECH-BALLON-Y))

        (.drawString g
          (cond
            (= (count sorted-drinking-days) 0) "my glass is empty."
            (< (count sorted-drinking-days) 3) "I'm only buzzed."
            (< (count sorted-drinking-days) 5) "I'm only tipsy."
            (< (count sorted-drinking-days) 7) "I'm  only slightly sloshed."
            :else "THE DRINKS, THEY DO NOTHING!")
          (+ 15 SPEECH-BALLON-X) (+ 30 SPEECH-BALLON-Y))

        (.drawLine g CALENDAR-LEFT-X CALENDAR-TOP-Y PANEL-WIDTH CALENDAR-TOP-Y )

        (loop [x 0
               days all-days
               day-number 1]
          (if (contains? drinking-days-set day-number)
            (.drawImage g beer-image x CALENDAR-TOP-Y nil))
          
          (.drawLine g x CALENDAR-TOP-Y x (+ 1 CALENDAR-BOTTOM-Y))
          (.drawString g (str (first days)) (+ x 10) (+ 15 CALENDAR-TOP-Y ))

          (if (< x CALENDAR-RIGHT-X)
            (recur (+ x CALENDAR-DAY-WIDTH ) (rest days) (+ 1 day-number))))))))

(defn create-display [sorted-drinking-days]

  (let [frame (new JFrame "Chuck is a' drinkin'!")
        panel (create-panel sorted-drinking-days)]

    (doto frame
      (.setDefaultCloseOperation (JFrame/EXIT_ON_CLOSE))
      (.add panel)
      (.setSize PANEL-WIDTH PANEL-HEIGHT)
      (.setVisible true))))


