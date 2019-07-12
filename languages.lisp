;;;; languages.lisp -*- coding: utf-8-unix; -*-

(in-package :cl-ygo)

(defvar *locale-string* nil)

(defvar *lang-seq* '(:zh-cn :zh-tw :en-us :ja-jp))

(defun init-locale-string (&rest categories)
  (setq *locale-string* nil)
  (loop for key in categories do
       (setf (getf *locale-string* key) nil)))

(defun add-locale-string-item (category item lang-code string)
  (pushnew (cons lang-code string)
	   (getf (getf *locale-string* category) item)
	   :test #'equal))

(defun get-locale-string-item (category item lang-code)
  (cdr (assoc lang-code
	      (getf (getf *locale-string* category) item))))

(defmacro with-category (category action sub-category &rest args)
  (cond				;TODO: add other action like remove...
    ((equal action 'add)
     `(progn
	,@(loop
	     for (lang string) on args by #'cddr
	     collect `(add-locale-string-item ,category ,sub-category
					      ,lang ,string))))
    (t ``t)))

(init-locale-string :card-category
		    :monster-category
		    :spell-category
		    :trap-category
		    :attribute :type
		    :race :zone :summon
		    :action :phase :other)

(with-category :card-category add :monster 
	       :zh-cn "怪兽"
	       :zh-tw "怪獸"
	       :ja-jp "モンスタ")

(with-category :card-category add :spell 
	       :zh-cn "魔法"
	       :zh-tw "魔法"
	       :ja-jp "魔法")

(with-category :card-category add :trap 
	       :zh-cn "陷阱"
	       :zh-tw "陷阱"
	       :ja-jp "罠")

(with-category :monster-category add :normal 
	       :zh-cn "通常"
	       :zh-tw "通常"
	       :ja-jp "通常")

(with-category :monster-category add :effect 
	       :zh-cn "效果"
	       :zh-tw "效果"
	       :ja-jp "効果")

(with-category :monster-category add :fusion 
	       :zh-cn "融合"
	       :zh-tw "融合"
	       :ja-jp "融合")

(with-category :monster-category add :ritual 
	       :zh-cn "仪式"
	       :zh-tw "儀式"
	       :ja-jp "儀式")

(with-category :monster-category add :tuner 
	       :zh-cn "调整"
	       :zh-tw "協調"
	       :ja-jp "チューナー")

(with-category :monster-category add :synchro 
	       :zh-cn "同调"
	       :zh-tw "同步"
	       :ja-jp "シンクロ")

(with-category :monster-category add :xyz 
	       :zh-cn "超量"
	       :zh-tw "超量"
	       :ja-jp "エクシーズ")

(with-category :monster-category add :pendulum 
	       :zh-cn "灵摆"
	       :zh-tw "靈擺"
	       :ja-jp "ペンデュラム")

(with-category :monster-category add :trap 
	       :zh-cn "陷阱怪兽"
	       :zh-tw "陷阱怪獸"
	       :ja-jp "罠モンスター")

(with-category :monster-category add :spirit 
	       :zh-cn "灵魂"
	       :zh-tw "靈魂"
	       :ja-jp "スピリット")

(with-category :monster-category add :union 
	       :zh-cn "同盟"
	       :zh-cn "聯合"
	       :ja-jp "ユニオン")

(with-category :monster-category add :gemini 
	       :zh-cn "二重"
	       :zh-tw "二重"
	       :ja-jp "デュアル")

(with-category :monster-category add :flip 
	       :zh-cn "反转"
	       :zh-tw "反轉"
	       :ja-jp "リバース")

(with-category :monster-category add :toon 
	       :zh-cn "卡通"
	       :zh-tw "卡通"
	       :ja-jp "トゥーン")

(with-category :monster-category add :token 
	       :zh-cn "衍生物"
	       :zh-tw "代幣"
	       :ja-jp "トークン")

(with-category :spell-category add :normal 
	       :zh-cn "通常"
	       :zh-tw "通常"
	       :ja-jp "通常")

(with-category :spell-category add :quick-play 
	       :zh-cn "速攻"
	       :zh-tw "速攻"
	       :ja-jp "速攻")

(with-category :spell-category add :continuous 
	       :zh-cn "永续"
	       :zh-tw "永續"
	       :ja-jp "永続")

(with-category :spell-category add :equip 
	       :zh-cn "装备"
	       :zh-tw "裝備"
	       :ja-jp "装備")

(with-category :spell-category add :field 
	       :zh-cn "场地"
	       :zh-tw "場地"
	       :ja-jp "フィールド")

(with-category :spell-category add :ritual 
	       :zh-cn "仪式"
	       :zh-tw "儀式"
	       :ja-jp "儀式")

(with-category :trap-category add :normal 
	       :zh-cn "通常"
	       :zh-tw "通常"
	       :ja-jp "通常")

(with-category :trap-category add :continuous 
	       :zh-cn "永续"
	       :zh-tw "永續"
	       :ja-jp "永続")

(with-category :trap-category add :counter 
	       :zh-cn "反击"
	       :zh-tw "反擊"
	       :ja-jp "カウンター")

(with-category :attribute add :earth 
	       :zh-cn "地"
	       :zh-tw "地"
	       :ja-jp "地")

(with-category :attribute add :water
	       :zh-cn "水")

(with-category :attribute add :fire
	       :zh-tw "炎"
	       :ja-jp "炎")

(with-category :attribute add :wind
	       :zh-tw "風"
	       :ja-jp "風")

(with-category :attribute add :light
	       :zh-tw "光"
	       :ja-jp "光")

(with-category :attribute add :dark
	       :zh-tw "闇"
	       :ja-jp "闇")

(with-category :attribute add :divine
	       :zh-tw "神"
	       :ja-jp "神")

(with-category :type add :warrior
	       :zh-tw "戰士"
	       :ja-jp "戦士族")

(with-category :type add :spellcaster
	       :zh-tw "魔法使"
	       :ja-jp "魔法使い族")

(with-category :type add :fairy
	       :zh-tw "天使"
	       :ja-jp "天使族")

(with-category :type add :fiend
	       :zh-tw "惡魔"
	       :ja-jp "悪魔族")

(with-category :type add :zombie
	       :zh-tw "不死"
	       :ja-jp "アンデット族")

(with-category :type add :machine
	       :zh-tw "機械"
	       :ja-jp "機械族")

(with-category :type add :aqua
	       :zh-tw "水"
	       :ja-jp "水族")

(with-category :type add :pyro
	       :zh-tw "炎"
	       :ja-jp "炎族")

(with-category :type add :rock
	       :zh-tw "岩石"
	       :ja-jp "岩石族")

(with-category :type add :winged
	       :zh-tw "鳥獸"
	       :ja-jp "鳥獣族")

(with-category :type add :plant
	       :zh-tw "植物"
	       :ja-jp "植物族")

(with-category :type add :insect
	       :zh-tw "昆蟲"
	       :ja-jp "昆虫族")

(with-category :type add :thunder
	       :zh-tw "雷"
	       :ja-jp "雷族")

(with-category :type add :dragon
	       :zh-tw "龍"
	       :ja-jp "ドラゴン族")

(with-category :type add :beast
	       :zh-tw "獸"
	       :ja-jp "獣族")

(with-category :type add :beast-warrior
	       :zh-tw "獸戰士"
	       :ja-jp "獣戦士族")

(with-category :type add :dinosaur
	       :zh-tw "恐龍"
	       :ja-jp "恐竜族")

(with-category :type add :fish
	       :zh-tw "魚"
	       :ja-jp "魚族")

(with-category :type add :sea-serpent
	       :zh-tw "海龍"
	       :ja-jp "海竜族")

(with-category :type add :reptile
	       :zh-tw "爬蟲類"
	       :ja-jp "爬虫類族")

(with-category :type add :psychic
	       :zh-tw "超能"
	       :ja-jp "サイキック族")

(with-category :type add :divine-beast
	       :zh-tw "幻神獸"
	       :ja-jp "幻神獣族")

(with-category :type add :creator-god
	       :zh-tw "創造神"
	       :ja-jp "創造神族")

(with-category :type add :wyrm
	       :zh-tw "幻龍"
	       :ja-jp "幻竜族")

(with-category :zone add :deck
	       :zh-tw "牌組"
	       :ja-jp "デッキ")

(with-category :zone add :hand
	       :zh-tw "手牌"
	       :ja-jp "手札")

(with-category :zone add :monster
	       :zh-tw "怪獸區"
	       :ja-jp "モンスターゾーン")

(with-category :zone add :spell&trap
	       :zh-tw "魔法陷阱區"
	       :ja-jp "魔法＆罠ゾーン")

(with-category :zone add :graveyard
	       :zh-tw "墓地"
	       :ja-jp "墓地")

(with-category :zone add :banished
	       :zh-tw "除外區"
	       :ja-jp "除外ゾーン")

(with-category :zone add :extra
	       :zh-tw "額外牌組"
	       :ja-jp "エクストラデッキ")

(with-category :zone add :overlay
	       :zh-tw "超量素材"
	       :ja-jp "Ｘ素材")

(with-category :zone add :field
	       :zh-tw "場地區"
	       :ja-jp "フィールドゾーン")

(with-category :zone add :pendulum
	       :zh-tw "靈擺區"
	       :ja-jp "ペンデュラムゾーン")

(with-category :summon add :normal
	       :zh-tw "通常召喚"
	       :ja-jp "通常召喚")

(with-category :summon add :special
	       :zh-tw "特殊召喚"
	       :ja-jp "特殊召喚")

(with-category :summon add :flip
	       :zh-tw "反轉召喚"
	       :ja-jp "反転召喚")

(with-category :summon add :advance
	       :zh-tw "升級召喚"
	       :ja-jp "アドバンス召喚")

(with-category :action add :release
	       :zh-tw "解放"
	       :ja-jp "リリース")

(with-category :action add :activate
	       :zh-tw "發動"
	       :ja-jp "発動")

(with-category :action add :remove-counter
	       :zh-tw "移除計數器"
	       :ja-jp "カウンターを取り除いて")

(with-category :action add :pay-lp
	       :zh-tw "支付生命值"
	       :ja-jp "LPを支払って")

(with-category :action add :remove-material
	       :zh-tw "移除(超量)的素材"
	       :ja-jp "素材を取り除いて")

(with-category :action add :reveal
	       :zh-tw "確認/公開"
	       :ja-jp "カードをめくる")
