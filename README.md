# cl-ygo
Common Lisp 版本的 YGO 。

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
3. 解析 ydk 文件: `(parse-deck "deck name")` 。 \
   例：假设一个 ydk 文件的内容如下：
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
   经过解析将会返回如下的 lisp 属性表：
   ```commonlisp
   '(:deck (card id 1 card id 2 )
     :extra (card id 3 card id 4) :side (card id 5))
   ```
4. 利用 ydk 文件的内容初始化牌组： `(init-deck "deck name")` 。 \
   如上的例子将会返回如下的 lisp 属性表：
   ```commonlisp
   '(:deck (#<card 1> #<card 2>)
     :extra (#<card 3> #<card 4>))
   ```
5. 根据卡片编号获取卡片信息并返回一个初始化后的对象实例：`(get-card-by-id id)`。 \
   
