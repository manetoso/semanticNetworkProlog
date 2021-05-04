:- dynamic  y/1.
:- dynamic n/1.

:- use_module(library(pce)).
:- use_module(library(pce_util)).
:- use_module(library(pce_style_item)).
:- pce_image_directory('./img').
:- dynamic color/2.

%-----------------------------IMAGENES-----------------------------
resource(imgAsw, image, image('answer.jpg')).
resource(imgCir, image, image('circle.jpg')).
resource(bk, image, image('bk.jpg')).
resource('1', image, image('aguila.jpg')).
resource('2', image, image('avestruz.jpg')).
resource('3', image, image('buho.jpg')).
resource('4', image, image('buitre.jpg')).
resource('5', image, image('cacatua.jpg')).
resource('6', image, image('carpintero.jpg')).
resource('7', image, image('cuervo.jpg')).
resource('8', image, image('ganso.jpg')).
resource('9', image, image('loro.jpg')).
resource('10', image, image('paloma.jpg')).
resource('11', image, image('pato.jpg')).
resource('12', image, image('pinzon.jpg')).
resource('13', image, image('tortolia.jpg')).
resource('14', image, image('tucan.jpg')).

%-----------------------------OPERADORES-----------------------------

 :- op(1115, fx, si).
 :- op(1110, xfy, entonces).
 :- op(1100, xfy, o).
 :- op(1000, xfy, y).
 :- op(900, fx, no).

 %-----------------------------REGLAS-----------------------------
regla(si es_una_ave_acuatica(Animal) y tiene_los_dedos_adheridos(Animal) entonces anseriforme(Animal)).

regla(si es_una_ave_pequeña(Animal) y tiene_tres_dedos_adelante_y_uno_atras(Animal) entonces paseriforme(Animal)).

regla(si es_nocturna(Animal) entonces strigiforme(Animal)).

regla(si tiene_dos_dedos_adelante_y_uno_atras(Animal) y es_una_ave_pequeña(Animal) entonces psitaciforme(Animal)).

regla(si su_alimento_es_variado(Animal) y es_buena_voladoras(Animal) entonces columbiforme(Animal)).

regla(si es_insectivoro(Animal) y tiene_dos_dedos_adelante_y_uno_atras(Animal) entonces piciforme(Animal)).

regla(si tiene_garras_poderosas(Animal) y es_ave_de_caseria(Animal) entonces falconiforme(Animal)).

regla(si no_vuela(Animal) y es_muy_grande(Animal) entonces struthianiforme(Animal)).

regla(si anseriforme(Animal) entonces pato(Animal)).
regla(si anseriforme(Animal) entonces cisne(Animal)).

regla(si paseriforme(Animal) entonces cuervo(Animal)).
regla(si paseriforme(Animal) entonces pinzon(Animal)).

regla(si strigiforme(Animal) entonces buho(Animal)).

regla(si psitaciforme(Animal) entonces loro(Animal)).
regla(si psitaciforme(Animal) entonces cacatua(Animal)).

regla(si columbiforme(Animal) entonces paloma(Animal)).
regla(si columbiforme(Animal) entonces tortolia(Animal)).

regla(si piciforme(Animal) entonces tucan(Animal)).
regla(si piciforme(Animal) entonces carpintero(Animal)).

regla(si falconiforme(Animal) entonces buitre(Animal)).
regla(si falconiforme(Animal) entonces aguila(Animal)).

regla(si struthianiforme(Animal) entonces avestrus(Animal)).

 %-----------------------------HIPOTESIS-----------------------------

clase_ave(anseriforme) :-
      anseriforme,
      !.

clase_ave(paseriforme) :-
      paseriforme,
      !.

clase_ave(strigiforme) :-
      strigiforme,
      !.

clase_ave(psitaciforme) :-
      psitaciforme,
      !.

clase_ave(columbiforme) :-
      columbiforme,
      !.

clase_ave(piciforme) :-
      piciforme,
      !.

clase_ave(falconiforme) :-
      falconiforme,
      !.

clase_ave(struthianiforme) :-
      struthianiforme,
      !.

hipotesis(pato) :- 
      pato,
      !.

hipotesis(cisne) :- 
      cisne,
      !.

hipotesis(cuervo) :- 
      cuervo,
      !.

hipotesis(pinzon) :- 
      pinzon,
      !.

hipotesis(buho) :- 
      buho,
      !.

hipotesis(loro) :- 
      loro,
      !.

hipotesis(cacatua) :- 
      cacatua,
      !.

hipotesis(paloma) :- 
      paloma,
      !.

hipotesis(tortilia) :- 
      tortilia,
      !.

hipotesis(tucan) :- 
      tucan,
      !.

hipotesis(carpintero) :- 
      carpintero,
      !.

hipotesis(aguila) :- 
      aguila,
      !.

hipotesis(buitre) :- 
      buitre,
      !.

hipotesis(avestrus) :- 
      avestrus,
      !.

hipotesis(desconocido).     /* sin diagnostico */

%-----------------------------VERIFICAR REGLAS-----------------------------
anseriforme :-
      verify(es_una_ave_acuatica),
      verify(tiene_los_dedos_adheridos).

paseriforme :-
      verify(es_una_ave_pequeña).
      verify(tiene_tres_dedos_adelante_y_uno_atras).

strigiforme :-
      verify(es_nocturna).

psitaciforme :-
      verify(tiene_dos_dedos_adelante_y_uno_atras),
      verify(es_una_ave_pequeña).

columbiforme :-
      verify(su_alimento_es_variado),
      verify(es_buena_voladoras).

piciforme :-
      verify(es_insectivoro).
      verify(tiene_dos_dedos_adelante_y_uno_atras).

falconiforme :-
      verify(tiene_garras_poderosas),
      verify(es_ave_de_caseria).

struthianiforme :-
      verify(no_vuela),
      verify(es_muy_grande).

pato :-
      anseriforme,
      verify(tiene_patas_cortas),
      verify(tiene_pico_con_punta_ancha),
      !.

cisne :-
      anseriforme,
      verify(tiene_mancha_negra_en_la_cara),
      verify(tiene_cuello_largo),
      !.

cuervo :-
      paseriforme,
      verify(tiene_plumas_negras),
      verify(tiene_pico_negro),
      !.

pinzon :-
      paseriforme,
      verify(tiene_pico_robusto),
      verify(tiene_plumaje_colorido),
      !.

buho :-
      strigiforme,
      verify(tiene_alas_largas),
      verify(son_solitarios),
      !.

loro :-
      psitaciforme,
      verify(tiene_pico_curvo),
      verify(tiene_cuatro_dedos_por_pata),
      !.

cacatua :-
      psitaciforme,
      verify(tiene_penacho_de_plumas),
      verify(es_predominantemente_blanca),
      !.
      
paloma :-
      columbiforme,
      verify(tiene_cuerpo_y_cuello_robusto),
      verify(tiene_nidos_debiles),
      !.

tortilia :-
      columbiforme,
      verify(es_pequeño),
      verify(es_predominantemente_gris),
      !.

tucan :-
      piciforme,
      verify(tiene_plumas_de_colores),
      verify(tiene_pico_de_colores),
      !.

carpintero :-
      piciforme,
      verify(perfora_troncos),
      verify(trepa_arboles),
      !.

buitre :-
      falconiforme,
      verify(tiene_un_tamaño_considerable),
      verify(sin_plumas_en_la_cara),
      !.

aguila :-
      falconiforme,
      verify(tiene_muy_buena_vista),
      verify(es_veloz_volando),
      !.

avestrus :-
      struthianiforme,
      verify(corre_rapido),
      verify(tiene_patas_fuertes),
      !.

%-----------------------------SALIDA AL EDITOR-----------------------------
ask(Question) :-
    write('¿El ave tiene la siguiente caracteristica : '),
    write(Question),
    write('? '),
    read(Response),
    nl,
    ( (Response == yes ; Response == y)
      ->
       assert(y(Question)) ;
       assert(n(Question)), fail).

verify(S) :-
   (y(S)
    ->
    true ;
    (n(S)
     ->
     fail ;
     ask2(S))).

undo :- 
      retract(y(_)),
      fail.
undo :- 
      retract(n(_)),
      fail.
      undo.

%-----------------------------MAIN-----------------------------
identificar :- 
      hipotesis(Animal),
      clase_ave(TipoAve),
      write('Pienso que el tipo de ave que describe es: '),
      write(TipoAve),
      nl,
      write('Y pienso que la ave específica que describe es: '),
      write(Animal),
      nl,
      undo.

%-----------------------------INTERFAZ-----------------------------
main:-
	new(Menu, dialog('S.E.A.')),
      image(Menu, bk, 0, 0),

	new(@L, text('Sistema experto de aves')),
      /*send(Menu, append, @L),*/
      send(@L, font, bold),
      /*send(L, alignment, center),*/
      send(Menu, display,@L,point(200,10)),

      image(Menu, imgCir, 40, 72),

	new(@texto, text('Segun las respuestas dadas tendra un resultado')),
      send(Menu, display,@texto,point(50,40)),

      ans_image(Menu, imgAsw),

      new(@resp1, label(nombre, '')),
      send(Menu, display,@resp1,point(150,80)),

	new(@resp2, label(nombre, '')),
      send(Menu, display,@resp2,point(300,80)),

      new(@texto2, text(
            'De forma siguiente se listan los tipos de aves que reconoce\nel sistema experto. Haga click en alguno de los botónes para\nobtener más información.'
      )),
      send(Menu, display, @texto2, point(50,120)),

	new(@boton, button('Realizar test', message(@prolog, botones))),
      send(Menu, display,@boton,point(20,450)),

      new(Salir, button('SALIR', and(message(Menu, destroy),message(Menu,free)))),
      send(Menu, display,Salir,point(380,450)),

	new(@btnTipo1, button('Anseriforme', message(@prolog, tipoAve1))),
	new(@btnTipo2, button('Paseriforme', message(@prolog, tipoAve2))),
	new(@btnTipo3, button('Strigiforme', message(@prolog, tipoAve3))),
	new(@btnTipo4, button('Psitaciforme', message(@prolog, tipoAve4))),
	new(@btnTipo5, button('Columbiforme', message(@prolog, tipoAve5))),
	new(@btnTipo6, button('Piciforme', message(@prolog, tipoAve6))),
	new(@btnTipo7, button('Falconiforme', message(@prolog, tipoAve7))),
	new(@btnTipo8, button('Struthianiforme', message(@prolog, tipoAve8))),

      image(Menu, '1', 144, 320),

	send(Menu, display, @btnTipo1, point(80,180)),
	send(Menu, display, @btnTipo2, point(210,180)),
	send(Menu, display, @btnTipo3, point(340,180)),
	send(Menu, display, @btnTipo4, point(80,220)),
	send(Menu, display, @btnTipo5, point(210,220)),
	send(Menu, display, @btnTipo6, point(340,220)),
	send(Menu, display, @btnTipo7, point(80,260)),
	send(Menu, display, @btnTipo8, point(210,260)),
	send(Menu, open_centered).

botones :-
      lim,
      hipotesis(Animal),
      clase_ave(TipoAve),
      send(@resp1, selection(TipoAve)),
      send(@resp2, selection(Animal)),
      new(@boton, button('inicia',message(@prolog,botones))),
      send(Menu, display,@boton,point(50,50)),
      lim,
      undo.

lim :-
      send(@resp1,selection('')).

ask2(Question) :-
      new(Di,dialog('Cuestionario para adivinar ave')),

      random(1, 14, NumRand),
      atom_string(ParsNum,NumRand),
      image(Di, ParsNum, 1, 3),

      new(L2,label(texto,'¿El ave tiene la siguiente caracteristica : ')),
      send(Di,append(L2)),

      new(Q,label(prob,Question)),
      send(Di,append(Q)),

      new(L3,label(texto,'?')),
      send(Di,append(L3)),

      new(B1,button(si,and(message(Di,return,yes)))),
      send(Di,append(B1)),

      new(B2,button(no,and(message(Di,return,no)))),
      send(Di,append(B2)),

      send(Di,open_centered),
      get(Di,confirm,Response),
      write(Response),
      send(Di,destroy),
      ( (Response == yes)
            ->
            assert(y(Question)) ;
            assert(n(Question)), fail).

tipoAve1 :-
      new(Menu, dialog('Tipo de ave: Anseriforme')),
      image(Menu, '11', 1, 1),
      new(Title, text('¿Cuáles son las aves Anseriforme?')),
      send(Menu, append, Title),
      send(Title, font, bold),
      send(Title, alignment, center),
      new(Content, text('Todas las especies en el orden están muy adaptadas\npara una existencia acuática y todas están preparadas\npara una natación eficaz(aunque algunas se han adaptado\na la tierra).')),
      send(Menu, append, Content),
      send(Content, alignment, left),
      send(Menu, open_centered).
tipoAve2 :-
      new(Menu, dialog('Tipo de ave: Paseriforme')),
      image(Menu, '7', 1, 1),
      new(Title, label(nombre, '¿Cuáles son las aves Paseriforme?')),
      send(Menu, append, Title),
      send(Title, font, bold),
      send(Title, alignment, center),
      new(Content, text('Son un gran orden de aves (el segundo más grande).\nEsto se debe a que se han diversificado demasiado\ngracias a las adaptaciones al medio muy complejas y\nvariadas que comprenden desde su capacidad para\nposarse, usos de sus cantos, inteligencia y la complejidad\ny diversidad de sus nidos.')),
      send(Menu, append, Content),
      send(Content, alignment, left),
      send(Menu, open_centered).
tipoAve3 :-
      new(Menu, dialog('Tipo de ave: Strigiforme')),
      image(Menu, '3', 1, 1),
      new(Title, label(nombre, '¿Cuáles son las aves Strigiforme?')),
      send(Menu, append, Title),
      send(Title, font, bold),
      send(Title, alignment, center),
      new(Content, text('Está compuesto de aves nocturnas y solitarias,\nson grnades cazadores y se alimentan principalmente\nde pequeños mamiferos, insectos y otras aves.')),
      send(Menu, append, Content),
      send(Content, alignment, left),
      send(Menu, open_centered).
tipoAve4 :-
      new(Menu, dialog('Tipo de ave: Psitaciforme')),
      image(Menu, '9', 1, 1),
      new(Title, label(nombre, '¿Cuáles son las aves Psitaciforme?')),
      send(Menu, append, Title),
      send(Title, font, bold),
      send(Title, alignment, center),
      new(Content, text('Son aves que habitan zonas tropicales y subtropicales.\nLa característica más común es que todos tienen\nun cuerpo similar al del loro; pico robusto y curvado,\npostura erguida y garras zigodáctilas (dos dedos enfrente\ny uno atrás).')),
      send(Menu, append, Content),
      send(Content, alignment, left),
      send(Menu, open_centered).
tipoAve5 :-
      new(Menu, dialog('Tipo de ave: Columbiforme')),
      image(Menu, '10', 1, 1),
      new(Title, label(nombre, '¿Cuáles son las aves Columbiforme?')),
      send(Menu, append, Title),
      send(Title, font, bold),
      send(Title, alignment, center),
      new(Content, text('Es el grupo de aves que incluye a las palomas, tórtolas\ny formas afines, así como es el grupo de los extintos\ndodos.')),
      send(Menu, append, Content),
      send(Content, alignment, left),
      send(Menu, open_centered).
tipoAve6 :-
      new(Menu, dialog('Tipo de ave: Piciforme')),
      image(Menu, '6', 1, 1),
      new(Title, label(nombre, '¿Cuáles son las aves Piciforme?')),
      send(Menu, append, Title),
      send(Title, font, bold),
      send(Title, alignment, center),
      new(Content, text('En general, estas aves son insectívoras, aunque los\ntucanes se alimentan principalmente de frutas, mientras\nque los indicadores comen cera de abejas.También\nson zigodáctilas (tienen dos dos enfrente y otro\natrás), lo cual sirve par aposarse en ramas de árboles\nmás fácilmente.')),
      send(Menu, append, Content),
      send(Content, alignment, left),
      send(Menu, open_centered).
tipoAve7 :-
      new(Menu, dialog('Tipo de ave: Falconiforme')),
      image(Menu, '1', 1, 1),
      new(Title, label(nombre, '¿Cuáles son las aves Falconiforme?')),
      send(Menu, append, Title),
      send(Title, font, bold),
      send(Title, alignment, center),
      new(Content, text('Estas aves tienen un cuerpo fuerte y compacto,\nmiembros robustos y cabeza voluminosa, casi redonda\ncon pico fuerte y cortante en forma de garfio.\nSon aves de presa conocidas como rapaces diurnas.')),
      send(Menu, append, Content),
      send(Content, alignment, left),
      send(Menu, open_centered).
tipoAve8 :-
      new(Menu, dialog('Tipo de ave: Struthianiforme')),
      image(Menu, '2', 1, 1),
      new(Title, label(nombre, '¿Cuáles son las aves Struthianiforme?')),
      send(Menu, append, Title),
      send(Title, font, bold),
      send(Title, alignment, center),
      new(Content, text('Son las aves paleognatas, no voladoras y con\nalgunas de ellas ya desaparecidas. Actualmente\nsolo agrupa a avestruces y formas fósiles\ncercanas, mientras que otros miembros fueron\nincluidos en nuevas familias.')),
      send(Menu, append, Content),
      send(Content, alignment, left),
      send(Menu, open_centered).

ans_image(Window, Image) :-
      new(Figure, figure),
      new(Bitmap, bitmap(resource(Image),@on)),
      send(Bitmap, name, 1),
      send(Figure, display, Bitmap),
      send(Figure, status, 1),
      send(Window, display, Figure, point(120,60)).

image(Window, Image, X, Y) :-
      new(Figure, figure),
      new(Bitmap, bitmap(resource(Image),@on)),
      send(Bitmap, name, 1),
      send(Figure, display, Bitmap),
      send(Figure, status, 1),
      send(Window, display, Figure, point(X,Y)).