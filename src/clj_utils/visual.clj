(ns ^{:doc "Visualization utility functions."}
  clj-util.visual
  (:use (incanter core stats datasets charts))
  (:import [org.jfree.chart ChartPanel JFreeChart])
  (:import [javax.swing JComponent JLabel JPanel])
  (:require [seesaw.core :as s])
  (:require [seesaw.font :as sf])
  (:require [clojure.inspector :as insp])
  (:import java.awt.Font)
  (:import java.awt.Color))

;; visualisation tools

;; https://gist.github.com/daveray/1441520
(s/native!)

(def frames
  (atom {}))

(defn change-bounds [left right]
  (let [dist (- right left)
        new-dist (* 1.02 dist)]
    [(- right new-dist)
     (+ left new-dist)]))

;; (defn adjust-chart-to-data [chart]
;;   (let [xyplot (doto (.getPlot chart)
;;                  (.setDomainCrosshairVisible true)
;;                  (.setRangeCrosshairVisible true))
;;         (.getDataset xyplot %)
;;         xysereries-s (map
;;                       #(only-one (.getSeries ))
;;                       (range (.getDatasetCount xyplot)))
;;         [x0 x1] (change-bounds (apply min
;;                                       (map #(.getMinX %) xysereries-s))
;;                                (apply max
;;                                       (map #(.getMaxX %) xysereries-s)))
;;         [y0 y1] (change-bounds (apply min
;;                                       (map #(.getMinY %) xysereries-s))
;;                                (apply max
;;                                       (map #(.getMaxY %) xysereries-s)))]
;;     (println [x0 x1 y0 y1])
;;     (.setRange (.getDomainAxis xyplot)
;;                x0 x1)
;;     (.setRange (.getRangeAxis xyplot)
;;                y0 y1)))

(defn adjust-chart-to-most-data [chart margin-ratio]
  (let [xyplot (doto (.getPlot chart)
                 (.setDomainCrosshairVisible true)
                 (.setRangeCrosshairVisible true))
        xydatasets (map #(.getDataset xyplot %)
                        (range (.getDatasetCount xyplot)))
        xs (for [xydataset xydatasets
                 series-idx (range (.getSeriesCount xydataset))
                 item-idx (range (.getItemCount xydataset series-idx))]
             (.getX xydataset series-idx item-idx))
        ys (for [xydataset xydatasets
                 series-idx (range (.getSeriesCount xydataset))
                 item-idx (range (.getItemCount xydataset series-idx))]
             (.getY xydataset series-idx item-idx))
        [x0 x1] (change-bounds (apply min xs)
                               (apply max xs))
        [y0 y1] (apply change-bounds (quantile ys :probs [margin-ratio
                                                          (- 1 margin-ratio)]))]
    (println [x0 x1 y0 y1])
    (.setRange (.getDomainAxis xyplot)
               x0 x1)
    (.setRange (.getRangeAxis xyplot)
               y0 y1)))


(defn sdisplay [frame-key content controls]
  (let [frame (do (let [existing-frame (@frames frame-key)]
                    (if existing-frame
                      (do
                        (println "found existing frame")
                        existing-frame)     ;
                      ;; else
                      (do (let [new-frame (s/frame)]
                            (println "adding new frame")
                            (swap! frames assoc frame-key new-frame)
                            new-frame)))))]
    (when (not (.isShowing frame))
      (-> frame s/pack! s/show!))
    (s/config! frame
               :content (s/border-panel
                         :north (s/horizontal-panel :items
                                                    (concat [(doto (s/label (str "  frame: " frame-key "   "))
                                                               (s/config! :background "#ddeeff"
                                                                          :foreground "#555555"
                                                                          :font (sf/font :name "ARIAL"
                                                                                         :style #{:bold}
                                                                                         :size 21)))]
                                                            (if controls
                                                              [controls]
                                                              ;;else
                                                              [])))
                         :center content))))


;; (defn new-chart-panel-with-controls [chart]
;;   (let [b (s/button :text "Auto Zoom")
;;         chart-panel (doto (ChartPanel. chart)
;;                       (.add b))]
;;     (s/listen b :action
;;               (fn [e] (.restoreAutoBounds chart-panel)))
;;     chart-panel))

;; (defn new-panel-with-chart-panel-and-controls [chart]
;;   (let [b (s/button :text "Auto Zoom")
;;         chart-panel (ChartPanel. chart)]
;;     (s/listen b :action
;;               (fn [e] (.restoreAutoBounds chart-panel)))
;;     (s/horizontal-panel :items
;;                         [chart-panel
;;                          b])))

(defn new-chart-panel-and-controls [chart]
  (let [b-auto-zoom (s/button :text "Auto Zoom")
        ;;b-data-zoom (s/button :text "Data Zoom")
        b-most-zoom (s/button :text "Most Zoom")
        chart-panel (ChartPanel. chart)]
    (..  chart (getPlot) (getRangeAxis) (setAutoRangeIncludesZero false))
    (.setFont (.getTitle chart)
              (sf/font :name "ARIAL"
                       :style #{:bold}
                       :size 14))
    (s/listen b-auto-zoom :action
              (fn [e] (.restoreAutoBounds chart-panel)))
    ;; (s/listen b-data-zoom :action
    ;;           (fn [e] (adjust-chart-to-data chart)))
    (s/listen b-most-zoom :action
              (fn [e] (adjust-chart-to-most-data chart 0.001)))
    {:chart-panel chart-panel
     :controls [b-auto-zoom
                ;;b-data-zoom
                b-most-zoom]}))

(defn new-panel-with-chart-panel-and-controls [chart]
  (let [chart-panel-and-controls (new-chart-panel-and-controls chart)]
    (s/vertical-panel
     :items [(:chart-panel chart-panel-and-controls)
             (s/horizontal-panel
              :items (:controls chart-panel-and-controls))])))

(defn show-chart [frame-key chart]
  (sdisplay frame-key
            (new-panel-with-chart-panel-and-controls chart)
            nil
            ))

(defn show-charts [frame-key charts]
  (sdisplay frame-key
            (s/vertical-panel :items
                              (map new-panel-with-chart-panel-and-controls
                                   charts))
            nil))

(defn scatter-plot-wrt-index [x]
  (scatter-plot (range (count x))
                x))

(defn xy-plot-wrt-index [x]
  (xy-plot (range (count x))
           x))


(defn xy-plots-wrt-index [adataset]
  (->> (col-names adataset)
       (map #($ % adataset))
       (map xy-plot-wrt-index)
       ))

(defn time-series-plots [adataset]
  (let [t ($ :time adataset)]
    (map #(time-series-plot 
           t
           ($ % adataset)
           :title (name %)
           :y-label ""
           )
         (filter
          (complement #(= % :time))
          (col-names adataset)))))

(defn show-plot [frame-key x]
  (show-chart frame-key (scatter-plot-wrt-index x)))




;;___________________________________________________________________________________
;; inspection tools

;; http://mnzk.hatenablog.com/entry/2013/05/01/020635


(defn find-children
  [^java.awt.Component co ^Class cls]
  (some->> co .getComponents
           (filter #(instance? cls %))))

(defn inspect
  [data]
  (let [font ;;(Font. "VL Gothic" Font/PLAIN 20)
        (sf/font :name "Arial"
                 :style #{:bold}
                 :size 18)
        frame (doto (insp/inspect-tree data)
                (.setVisible false))
        jtree  (doto
                   (-> frame .getContentPane
                       (find-children javax.swing.JScrollPane) first
                       (find-children javax.swing.JViewport) first
                       (find-children javax.swing.JTree) first)
                 (.setFont font)
                 (.setBackground java.awt.Color/LIGHT_GRAY))]
    (doto frame
      ;;(.setSize 1000 800)
      (.setVisible true))))


