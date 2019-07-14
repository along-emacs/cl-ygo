# cl-ygo
Common Lisp 版本的 YGO 。

## 组织
目录结构与项目：[ygopro](https://github.com/Fluorohydride/ygopro) 一致，因为需要引用其 `cards.cdb` 与卡牌的图片。

## 功能
1. 定义了全局的卡牌缓存`*cards-cache*`。\
   初始化卡组时，这里会存放每个对象的引用。
2. 定义了存放各种位置的卡牌列表`*card-lists*`，比如卡组、手卡和墓地等。 \
   其中的对象与缓存中的对象是相同的引用。其具体格式为:
   ```commonlisp
   '(:deck ()      :hand () :extra ()
     :monster ()   :spell&trap ()
     :graveyard () :banished ()
     :field ()     :pendulum ())
   ```
3. 解析 `ydk` 文件: `(parse-deck "deck name")` 。 \
   例：假设一个 `example.ydk` 的内容如下：
   ```text
   #created by ...
   #main
   card id 1
   card id 2
   #extra
   card id 3
   card id 4
   !side
   card id 5
   ```
   经过解析将会返回如下的 `lisp` 属性表：
   ```commonlisp
   (parse-deck "example") =>
   '(:deck (card id 1 card id 2 )
     :extra (card id 3 card id 4) :side (card id 5))
   ```
4. 利用 `ydk` 文件的内容初始化牌组： `(init-deck "deck name")` 。 \
   如上的例子将会返回如下的 `lisp` 属性表：
   ```commonlisp
   (init-deck "example") =>
   '(:DECK (#<card 1> #<card 2>)
     :EXTRA (#<card 3> #<card 4>))
   ```
5. 根据卡片编号获取卡片信息并返回一个初始化后的对象实例：`(get-card-by-id id)`。
   - [x] 获取基本信息
   - [ ] 更新 `sql` 以获得更多信息
6. 根据参数获取指定区域的所有卡牌：`(get-cards-from [[:deck]|:hand :graveyard ...])`。 \
   可以加多个关键词参数，默认的区域参数是 `:deck` 。因为需要得知卡牌的来源，所以最后返回的结果将是 `lisp` 属性表：
   ```commonlisp
   (get-cards-from :deck :extra) =>
   '(:DECK (card id 1 card id 2 )
     :EXTRA (card id 3 card id 4))
   ```
7. 在指定的位置根据名字检索卡牌：`(search-cards-by-name "card name" [[:deck]|:hand :extra ...])`。 \
   同样可以加多个指定区域的参数。如果不存在则返回 `nil`：
   ```commonlisp
   (search-cards-by-name "青眼" :deck :extra :hand) =>
   ((:DECK
    (#<青眼白龙>  #<青眼白龙>  #<青眼白龙>  #<青眼亚白龙>  #<青眼亚白龙>  #<青眼亚白龙>  #<青眼混沌极龙>
     #<青眼混沌极龙>  #<青眼混沌龙> ))
    (:EXTRA (#<青眼双爆裂龙>  #<青眼精灵龙> ))
	(:HAND NIL))
   ```
8. TODO: 使用 `lisp` 风格的，且尽量接近自然语言的方式处理更多的检索条件。\
   比如要检索 `8星以下的龙族或魔法师族通常怪兽` 的代码如下：
   ```commonlisp
   (search-cards (and (or (race = :dragon)
                          (race = :spellcaster))
                      (level <= 8)
                      (type = :normal)))
   ```
9. TODO: 实现卡牌从一个区域移动到另一个区域。 \
   常见的有 `抽卡` 这个操作，比如：
   ```commomlisp
   (move-cards :deck :hand '(#<card1> #<card2>))
   <==>
   (draw 2)
   ```
10. TODO: 其他。
