# cl-ygo
Common Lisp �汾�� YGO ��

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
3. ���� ydk �ļ�: `(parse-deck "deck name")` �� \
   ��������һ�� ydk �ļ����������£�
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
   �����������᷵�����µ� lisp ���Ա�
   ```commonlisp
   '(:deck (card id 1 card id 2 )
     :extra (card id 3 card id 4) :side (card id 5))
   ```
4. ���� ydk �ļ������ݳ�ʼ�����飺 `(init-deck "deck name")` �� \
   ���ϵ����ӽ��᷵�����µ� lisp ���Ա�
   ```commonlisp
   '(:deck (#<card 1> #<card 2>)
     :extra (#<card 3> #<card 4>))
   ```
5. ���ݿ�Ƭ��Ż�ȡ��Ƭ��Ϣ������һ����ʼ����Ķ���ʵ����`(get-card-by-id id)`�� \
   
