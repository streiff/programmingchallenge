(import '(java.awt GridLayout)
        '(java.awt.event ActionListener)
        '(java.awt.image BufferedImage)
        '(java.io File)
        '(javax.imageio ImageIO)
        '(javax.swing JFrame JPanel JLabel JTextField JButton))

(defn create-panel [input-string image]
  (let [reversed-string (apply str (reverse input-string))]
    (println reversed-string)
    (proxy [JPanel] []
      (paintComponent [g]
        (doto g
          (.drawImage image, 0, 70, nil)
          (.drawString "Chuck Norris has hit your string!" 25 25)
          (.drawString (apply str (reverse input-string)) 25 45)
          (.drawString "It also fell so hard it dropped onto the console!" 25 65))))))

(let [frame (new JFrame "String Reverser | resreveR gnirtS")
      input-label (new JLabel "String: ")
      text-box (new JTextField "Chuck Norris!")
      button (new JButton "Hit it!")]

  (. button (addActionListener
    (proxy [ActionListener] []
      (actionPerformed [evt]
        (let [new-frame (new JFrame "Your String was hit!")
              image (ImageIO/read (new File "chuck.jpg"))
              panel (create-panel (. text-box getText) image)]
          (doto new-frame
            (.add panel)
            (.setSize 400 420)
            (.setVisible true)))))))

  (doto frame
    (.setDefaultCloseOperation (JFrame/EXIT_ON_CLOSE))
    (.setLayout (new GridLayout 3 1 1 1))
    (.add input-label)
    (.add text-box)
    (.add button)
    (.setSize 400 125)
    (.setVisible true))

  (. text-box selectAll))
