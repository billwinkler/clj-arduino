(ns clj-arduino.ov7670-img
  (:use clj-arduino.ov7670)
  (:require [clojure.java.io :as io]
            [clodiuno.core :refer [close]])
  (:import [java.awt.image BufferedImage DataBufferByte]
           [javax.imageio ImageIO]
           [mikera.gui Frames]))

(defn gray-image
  "for testing"
  [path shade]
  (let [arr (into-array Byte/TYPE (repeat (* 144 88) (byte shade)))
        image (BufferedImage. 88 144 BufferedImage/TYPE_BYTE_GRAY)
        data (-> image .getRaster .getDataBuffer .getData)]
    (System/arraycopy arr 0 data 0 (* 144 88))
    (ImageIO/write image, "jpg", (io/file path))))

(comment 
  (gray-image "resources/img_0.jpg" 0) ;; black
  (gray-image "resources/img_8.jpg" 8) ;; black
  (gray-image "resources/img_64.jpg" 64) ;; dark gray
  (gray-image "resources/img_127.jpg" 127) ;; medium gray
  (gray-image "resources/img_-128.jpg" -128) ;; medium gray
  (gray-image "resources/img_-64.jpg" -64) ;; light gray
  (gray-image "resources/img_-8.jpg" -8) ;; off white
  )

;; (defn- image-size
;;   "return width, height"
;;   [image]
;;   [(alength (aget image 0)) (alength image)])

;; (defn- flatten-img
;;   "flatten 2 dimensional image into single dim byte array"
;;   [image]
;;   (let [[w h] (image-size image)]
;;     (loop [i h
;;            bytez []]
;;       (if (zero? i) (->> bytez flatten reverse (into-array Byte/TYPE))
;;           (recur (dec i)
;;                  (conj bytez  
;;                        (loop [j w
;;                               row []]
;;                          (if (zero? j) row
;;                              (recur (dec j) (conj row (aget image (dec i) (dec j))))))))))))


(defn to-jpg
  "generate a jpg from the image"
  [image path]
  (let [;;image2 (BufferedImage. (img-size :w) (img-size :h) BufferedImage/TYPE_BYTE_GRAY)
        image2 (BufferedImage. 150 60 BufferedImage/TYPE_BYTE_GRAY)
        data (-> image2 .getRaster .getDataBuffer .getData)]
;;    (System/arraycopy image 0 data 0 (alength image))
    (System/arraycopy image 0 data 0 9000)
    (ImageIO/write image2, "jpg", (io/file path))))

(defn show
  "Display an image"
  [image]
  (let [[w h] [(img-size :w) (img-size :h)]
        image2 (BufferedImage. w h BufferedImage/TYPE_BYTE_GRAY)
        data (-> image2 .getRaster .getDataBuffer .getData)]
    (System/arraycopy image 0 data 0 (* w h))
    (Frames/display image2 "image capture")
    "ok"))

(comment
  @accum
  ;; pclk pre-scaller (11 seems to work best)
  (set-register-bits 0x11 11)
  ;; scaling pclk divider
  (set-register-bits 0x3E 2r00011010)
  (set-register-bits 0x3E 2r1 [4 4])
  (set-register-bits 0x3E 2r000 [2 0])
  (set-register-bits 0x73 2r000 [2 0])
  ;; MSB, LSB dummy pixel insert
  (set-register-bits 0x2A 0x00 [7 4])
  (set-register-bits 0x2B 0x00 [7 0])
  ;; 2E MSB, 2D dummy lines
  (set-register-bits 0x2E 0x00 [7 0])
  (set-register-bits 0x2D 60 [7 0])
  ;; negative image
  (set-register-bits 0x3A 2r1 [5 5])
  ;; swap UV
  (set-register-bits 0x3A 2r1 [3 3])
  (set-register-bits 0x3D 2r1 [0 0])

  ;; night mode
  (set-register-bits 0x3B 2r1 [7 7])  ;; enable
  (set-register-bits 0x3B 2r00 [6 5]) ;; minimum frame rate, normal
  (set-register-bits 0x3B 2r01 [6 5]) ;; 1/2 of normal frame rate


  (let [REG_COM8        0x13
        COM8_FASTAEC	0x80
        COM8_AECSTEP	0x40
        REG_GAIN	0x00
        REG_AECH	0x10
        REG_COM4	0x0d
        REG_COM9	0x14
        REG_BD50MAX	0xa5 
        REG_BD60MAX	0xab
        REG_AEW		0x24
        REG_AEB		0x25
        REG_VPT		0x26
        REG_HAECC1	0x9f
        REG_HAECC2	0xa0 
        REG_HAECC3	0xa6
        REG_HAECC4	0xa7
        REG_HAECC5	0xa8
        REG_HAECC6	0xa9
        REG_HAECC7	0xaa
        COM8_AGC	0x04
        COM8_AEC	0x01
        MAGIC_A1        0xa1]
    (set-register-bits REG_COM8 (bit-or COM8_FASTAEC COM8_AECSTEP))
    (set-register-bits REG_GAIN 0)
    (set-register-bits REG_AECH 0)
    (set-register-bits REG_COM4 0x40)
    (set-register-bits REG_COM9 0x18)
    (set-register-bits REG_BD50MAX 0x05)
    (set-register-bits REG_BD60MAX 0x07)
    (set-register-bits REG_AEW 0x95)
    (set-register-bits REG_AEB 0x33)
    (set-register-bits REG_VPT 0xe3)
    (set-register-bits REG_HAECC1 0x78)
    (set-register-bits REG_HAECC2 0x68)
    (set-register-bits MAGIC_A1 0x03)
    (set-register-bits REG_HAECC3 0xd8)
    (set-register-bits REG_HAECC4 0xd8)
    (set-register-bits REG_HAECC5 0xf0)
    (set-register-bits REG_HAECC6 0x90)
    (set-register-bits REG_HAECC7 0x94)    
    ;;    (set-register-bits REG_COM8 (bit-or COM8_FASTAEC COM8_AECSTEP COM8_AGC COM8_AEC))
    )


  ;; aec stuff
  ;; AGC and AEC parameters.  Note we start by disabling those features,
  ;;  then turn them only after tweaking the values. */
  ;; {REG_COM8, COM8_FASTAEC | COM8_AECSTEP},
  

  ;; {REG_GAIN, 0},	
  ;; {REG_AECH, 0},
  ;; {REG_COM4, 0x40}, /* magic reserved bit */
  ;; {REG_COM9, 0x18}, /* 4x gain + magic rsvd bit */
  ;; {REG_BD50MAX, 0x05},	
  ;; {REG_BD60MAX, 0x07},
  ;; {REG_AEW, 0x95},	
  ;; {REG_AEB, 0x33},
  ;; {REG_VPT, 0xe3},	
  ;; {REG_HAECC1, 0x78},
  ;; {REG_HAECC2, 0x68},	
  ;; {0xa1, 0x03}, /* magic */
  ;; {REG_HAECC3, 0xd8},	
  ;; {REG_HAECC4, 0xd8},
  ;; {REG_HAECC5, 0xf0},	
  ;; {REG_HAECC6, 0x90},
  ;; {REG_HAECC7, 0x94},
  ;; {REG_COM8, COM8_FASTAEC|COM8_AECSTEP|COM8_AGC|COM8_AEC},
  


  (to-jpg (:image @accum) "resources/ov7670_8mhz_dark.jpg")
  (to-jpg (:image @accum) "resources/ov7670_8mhz_light.jpg")
  (to-jpg (:image @accum) "resources/ov7670_1mhz_dark.jpg")
  (to-jpg (:image @accum) "resources/ov7670_1mhz_light.jpg")
  (open-board)
  (initialize)
  (close board)
  (set-register-bits 0x11 11)
  (cmd :capture-image)
  (exposure-time)
  (show (@accum :image))
  (to-jpg (:image @accum) "resources/ov7670_8mhz_light.jpg")
  (cmd :some-pinc-d)
  (cmd :clock-at-1mhz)
  (cmd :clock-at-8mhz)
  (cmd :vsync-timing)
  (cmd :count-pixels)
  (cmd :pckl-timing)
  (cmd :test)

  )

