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

(defn to-jpg
  "generate a jpg from the image"
  [image path]
  (let [[w h] [(img-size :w) (img-size :h)]
        image2 (BufferedImage. w h BufferedImage/TYPE_BYTE_GRAY)
        data (-> image2 .getRaster .getDataBuffer .getData)]
    (System/arraycopy image 0 data 0 (* w h))
    (ImageIO/write image2, "jpg", (io/file path))))

(defn show
  "Display an image"
  [image & title]
  (let [[w h] [(img-size :w) (img-size :h)]
        image2 (BufferedImage. w h BufferedImage/TYPE_BYTE_GRAY)
        data (-> image2 .getRaster .getDataBuffer .getData)
        title (or (first title) "image capture")]
    (System/arraycopy image 0 data 0 (* w h))
    (Frames/display image2 title)
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

  (to-jpg (:image @accum) "resources/ov7670_8mhz_dark.jpg")
  (to-jpg (:image @accum) "resources/ov7670_8mhz_light.jpg")
  (to-jpg (:image @accum) "resources/ov7670_1mhz_dark.jpg")
  (to-jpg (:image @accum) "resources/ov7670_1mhz_light.jpg")
  (open-board)
  (initialize)
  (close board)
  (pll-multiplier 1)
  (pll-multiplier)
;;  (set-register-bits 0x11 11)
  (night-mode)
  (night-mode false)
  (de-noise-mode true)
  
  (* 130 120)
  (exposure)
  (gain 1)
  (gain)
  (gamma 0.35)
  (gamma 0.4)
  (set-register-bits 0x11 63)
  (cmd :capture-image)
  (show (@accum :image))
  (to-jpg (:image @accum) "resources/ov7670_8mhz_light.jpg")
  (soft-reset!)
  (cmd :clock-at-1mhz)
  (cmd :clock-at-8mhz)
  (cmd :vsync-timing)
  (cmd :count-pixels)
  (cmd :pckl-timing)
  (cmd :test))

