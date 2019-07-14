# cl-ygo
Common Lisp �汾�� YGO ��

## ��֯
Ŀ¼�ṹ����Ŀ��[ygopro](https://github.com/Fluorohydride/ygopro) һ�£���Ϊ��Ҫ������ `cards.cdb` �뿨�Ƶ�ͼƬ��

## ����
1. ������ȫ�ֵĿ��ƻ���`*cards-cache*`��\
   ��ʼ������ʱ���������ÿ����������á�
2. �����˴�Ÿ���λ�õĿ����б�`*card-lists*`�����翨�顢�ֿ���Ĺ�صȡ� \
   ���еĶ����뻺���еĶ�������ͬ�����á�������ʽΪ:
   ```commonlisp
   '(:deck ()      :hand () :extra ()
     :monster ()   :spell&trap ()
     :graveyard () :banished ()
     :field ()     :pendulum ())
   ```
3. ���� `ydk` �ļ�: `(parse-deck "deck name")` �� \
   ��������һ�� `example.ydk` ���������£�
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
   �����������᷵�����µ� `lisp` ���Ա�
   ```commonlisp
   (parse-deck "example") =>
   '(:deck (card id 1 card id 2 )
     :extra (card id 3 card id 4) :side (card id 5))
   ```
4. ���� `ydk` �ļ������ݳ�ʼ�����飺 `(init-deck "deck name")` �� \
   ���ϵ����ӽ��᷵�����µ� `lisp` ���Ա�
   ```commonlisp
   (init-deck "example") =>
   '(:DECK (#<card 1> #<card 2>)
     :EXTRA (#<card 3> #<card 4>))
   ```
5. ���ݿ�Ƭ��Ż�ȡ��Ƭ��Ϣ������һ����ʼ����Ķ���ʵ����`(get-card-by-id id)`��
   - [x] ��ȡ������Ϣ
   - [ ] ���� `sql` �Ի�ø�����Ϣ
6. ���ݲ�����ȡָ����������п��ƣ�`(get-cards-from [[:deck]|:hand :graveyard ...])`�� \
   ���ԼӶ���ؼ��ʲ�����Ĭ�ϵ���������� `:deck` ����Ϊ��Ҫ��֪���Ƶ���Դ��������󷵻صĽ������ `lisp` ���Ա�
   ```commonlisp
   (get-cards-from :deck :extra) =>
   '(:DECK (card id 1 card id 2 )
     :EXTRA (card id 3 card id 4))
   ```
7. ��ָ����λ�ø������ּ������ƣ�`(search-cards-by-name "card name" [[:deck]|:hand :extra ...])`�� \
   ͬ�����ԼӶ��ָ������Ĳ���������������򷵻� `nil`��
   ```commonlisp
   (search-cards-by-name "����" :deck :extra :hand) =>
   ((:DECK
    (#<���۰���>  #<���۰���>  #<���۰���>  #<�����ǰ���>  #<�����ǰ���>  #<�����ǰ���>  #<���ۻ��缫��>
     #<���ۻ��缫��>  #<���ۻ�����> ))
    (:EXTRA (#<����˫������>  #<���۾�����> ))
	(:HAND NIL))
   ```
8. TODO: ʹ�� `lisp` ���ģ��Ҿ����ӽ���Ȼ���Եķ�ʽ�������ļ���������\
   ����Ҫ���� `8�����µ������ħ��ʦ��ͨ������` �Ĵ������£�
   ```commonlisp
   (search-cards (and (or (race = :dragon)
                          (race = :spellcaster))
                      (level <= 8)
                      (type = :normal)))
   ```
9. TODO: ʵ�ֿ��ƴ�һ�������ƶ�����һ������ \
   �������� `�鿨` ������������磺
   ```commomlisp
   (move-cards :deck :hand '(#<card1> #<card2>))
   <==>
   (draw 2)
   ```
10. TODO: ������
