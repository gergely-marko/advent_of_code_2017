module Day8 exposing (..)

import Html
import Dict exposing (Dict)


main : Html.Html msg
main =
    let
        result =
            execute instructions

        max_value =
            result
                |> Tuple.first
                |> Dict.values
                |> List.maximum
                |> Maybe.withDefault 0

        abs_max =
            result
                |> Tuple.second
    in
        Html.div
            []
            [ max_value |> toString |> Html.text
            , Html.text " / "
            , abs_max |> toString |> Html.text
            ]


type alias Instruction =
    { target_register : String
    , condition_register : String
    , condition : Int -> Bool
    , execute : Int -> Int
    }


execute : List Instruction -> ( Dict String Int, Int )
execute instructions =
    instructions
        |> List.foldl
            (\i ( registers, max_value ) ->
                let
                    target_value =
                        registers
                            |> Dict.get i.target_register
                            |> Maybe.withDefault 0

                    condition_value =
                        registers
                            |> Dict.get i.condition_register
                            |> Maybe.withDefault 0

                    new_value =
                        if i.condition condition_value then
                            i.execute target_value
                        else
                            target_value

                    new_max =
                        Basics.max max_value new_value
                in
                    registers
                        |> Dict.insert i.target_register new_value
                        |> (\r -> ( r, new_max ))
            )
            ( registers, 0 )


registers : Dict String Int
registers =
    instructions
        |> List.foldl
            (\i registers ->
                registers
                    |> Dict.insert i.target_register 0
                    |> Dict.insert i.condition_register 0
            )
            Dict.empty


instructions : List Instruction
instructions =
    input
        |> String.lines
        |> List.filter (\l -> String.length l > 0)
        |> List.map create_instruction


create_instruction : String -> Instruction
create_instruction line =
    case String.words line of
        [ target_register, delta_dir, delta_str, _, condition_register, condition_str, condition_value_str ] ->
            let
                delta_value =
                    delta_str |> String.toInt |> Result.withDefault 0

                condition_value =
                    condition_value_str |> String.toInt |> Result.withDefault 0

                condition =
                    case condition_str of
                        ">" ->
                            flip (>) condition_value

                        "<" ->
                            flip (<) condition_value

                        ">=" ->
                            flip (>=) condition_value

                        "<=" ->
                            flip (<=) condition_value

                        "==" ->
                            (==) condition_value

                        "!=" ->
                            (/=) condition_value

                        _ ->
                            Debug.crash "unknown condition symbol"

                execute =
                    if delta_dir == "inc" then
                        (+) delta_value
                    else
                        flip (-) delta_value
            in
                { target_register = target_register
                , condition_register = condition_register
                , condition = condition
                , execute = execute
                }

        _ ->
            Debug.crash "line parsing failed"


test_input =
    """
b inc 5 if a > 1
a inc 1 if b < 5
c dec -10 if a >= 1
c inc -20 if c == 10
"""


input =
    """
smi inc 781 if epx > -2
yrf dec -813 if jzm != 6
ben dec -383 if sp == 0
tlj dec -356 if sp <= 4
ssv dec -128 if tlj <= 360
vlh dec -978 if ih == 0
ben dec -28 if bwj > -5
w dec 216 if ben == 411
ke dec -540 if blg <= 9
ty dec 469 if yrf > 810
epx inc 203 if xo > -9
mlp inc -625 if blg != 10
sp dec -248 if epx == 203
xo inc 363 if oon >= -7
ke inc 188 if ih <= 9
smi dec -738 if smi == 781
epx inc 257 if ga >= 1
fdv inc 843 if rt >= -1
blg inc -99 if ssv == 128
oon dec 512 if vlh > 977
oon inc 233 if ben > 410
mx dec -577 if yrf >= 812
sp dec 919 if n <= 0
epx inc 639 if mx != 576
oon dec 628 if oon <= -271
sp dec 172 if rt > -4
b dec -207 if ty == -469
w inc -439 if ty <= -479
bwj dec 533 if sp >= -844
sp inc -882 if jzm == 0
ih inc -377 if b >= 199
k dec 546 if tlj <= 364
oon inc -617 if ih != -373
yrf dec -400 if fdv > 840
bwj dec -254 if ssv <= 134
ssv dec -314 if tlj >= 351
smi dec 725 if blg <= -93
oon inc 485 if rt < 4
jzm inc 498 if xo == 363
yrf dec -391 if b < 216
epx inc -282 if ssv == 442
tlj dec 840 if jzm < 505
mlp dec -632 if jzm < 504
ssv dec -440 if b <= 207
blg dec -499 if tlj <= -482
xo dec 734 if xo == 363
ben dec -553 if rt == 0
smi inc 41 if mx >= 574
w dec -995 if ke < 738
xo dec -465 if w < 774
sp dec 339 if smi == 835
mx dec -38 if rt > -4
ben inc -448 if blg >= 401
oon inc 272 if w > 770
mx inc -805 if ga < 4
sp dec 853 if epx == 560
fdv inc 165 if fdv > 838
yrf inc -605 if blg > 399
tlj dec 533 if bwj < -271
smi dec 915 if ben > 954
oon dec 864 if ben != 964
ssv dec -111 if yrf < 990
sp inc 401 if jzm != 497
xo dec -434 if tlj != -1017
vlh dec 650 if fdv < 1000
ssv inc -570 if rt < 6
fdv inc -788 if mlp <= 14
oon dec -121 if ssv > 319
ben dec 905 if k == -546
tlj inc 853 if bwj < -274
vlh dec -680 if b != 201
rt dec 661 if bwj != -276
epx dec -515 if ssv > 318
rt inc -214 if ih <= -372
k dec 827 if ben < 67
ih dec -708 if sp != -2525
w dec -616 if epx == 560
b dec -455 if b >= 207
sp dec -619 if vlh > 1662
bwj dec -645 if ssv >= 305
n dec -276 if ke <= 731
w inc -426 if epx >= 566
k inc 848 if rt > -881
mx inc -952 if fdv > 219
ga dec 423 if mx > -1143
w inc 37 if bwj <= 373
jzm inc -800 if ben >= 56
xo inc -954 if xo >= -368
n inc -778 if jzm < -292
w inc 356 if tlj > -165
ty dec 913 if w > 1779
rt inc -810 if ke != 731
ke inc -838 if ssv <= 313
oon dec -839 if mx < -1139
xo dec 921 if jzm > -310
b dec 381 if jzm < -292
oon dec 273 if ssv <= 314
blg dec -524 if ga <= -415
ga inc 9 if rt >= -1694
rt dec -133 if smi <= -79
blg dec -518 if yrf >= 997
xo inc 718 if jzm >= -309
w inc -224 if smi != -77
epx dec 734 if n >= -502
ssv inc 770 if ben < 60
fdv inc 203 if ih <= 332
w dec -693 if tlj <= -162
rt inc 789 if smi <= -75
mx inc -359 if bwj >= 359
ty dec 178 if mlp != 3
ty dec -539 if k >= -532
w inc 769 if tlj == -164
k inc 91 if n == -502
bwj inc 730 if jzm == -302
ssv inc -895 if vlh == 1658
rt inc -440 if ih >= 340
bwj inc -38 if smi > -89
sp inc 718 if w == 3026
oon dec -533 if yrf < 1009
tlj dec -944 if xo == -574
vlh inc -608 if b > 277
ssv inc -88 if sp >= -1799
vlh dec -493 if bwj <= 1067
b inc 676 if ga <= -420
bwj inc 676 if k == -434
blg inc -240 if mlp <= 10
b dec 130 if xo > -580
blg dec -363 if k > -441
ty dec 903 if bwj >= 1743
ga dec -499 if fdv == 423
bwj dec -500 if ty > -1022
n inc -250 if jzm <= -308
jzm dec 3 if fdv == 423
epx dec -930 if n == -499
b dec 614 if bwj != 2234
blg inc -908 if n >= -502
ty dec -309 if oon <= 332
b dec -611 if ssv <= 107
yrf dec -707 if vlh > 1536
tlj inc 734 if b <= 756
smi dec -75 if jzm >= -306
n dec -390 if fdv >= 416
vlh dec 950 if sp >= -1788
oon dec -95 if ben >= 67
jzm inc -22 if epx == -174
epx dec 442 if jzm >= -331
ty inc -583 if sp >= -1798
mx inc -744 if vlh >= 1534
w dec -142 if ke > -101
ty dec -535 if bwj < 2243
xo dec 190 if n > -117
smi inc 358 if k == -434
vlh inc 321 if mlp >= 9
mx inc 325 if epx == -611
ke dec -631 if oon == 334
vlh dec 798 if k <= -427
w dec 183 if bwj > 2224
ssv dec -506 if fdv >= 432
fdv inc 125 if ssv == 99
epx inc -465 if vlh <= 754
ga dec 108 if mlp != 1
jzm dec -623 if oon == 332
ben inc -400 if vlh < 753
sp inc 157 if bwj < 2236
fdv dec -4 if xo == -774
mx dec -590 if ben > -347
ssv inc -542 if b <= 766
epx dec -189 if rt >= -766
ben dec 819 if ben >= -347
sp inc -194 if n < -116
n dec 755 if mlp != 16
ih dec -49 if mx < -1646
tlj inc 653 if ben <= -1160
ben inc 574 if n >= -873
fdv dec 346 if k != -427
ke inc -49 if blg == 657
epx inc -322 if bwj >= 2230
epx dec -984 if vlh >= 741
smi dec -486 if w <= 2846
mlp inc 992 if blg != 667
jzm dec 532 if bwj <= 2234
jzm dec 600 if sp == -1641
xo inc -795 if tlj < 1437
ben dec -93 if ke == -159
sp inc -226 if epx < -222
n dec 333 if b != 771
smi dec -731 if tlj != 1426
k dec 654 if jzm < -827
tlj dec -167 if ty == -760
n inc -905 if ga >= -32
w dec -561 if sp != -1864
b dec 710 if mlp >= 1002
ke dec 957 if tlj >= 1599
sp inc -272 if rt <= -754
blg inc -247 if oon == 332
ty inc 846 if mx < -1646
mlp inc -678 if bwj == 2234
vlh dec 254 if bwj == 2228
yrf inc 500 if ih == 380
mx inc 740 if blg > 401
yrf dec 692 if bwj == 2234
ke inc 949 if n == -2098
n dec 492 if fdv <= 209
ssv dec 335 if blg < 411
mx inc 835 if blg < 403
bwj dec -565 if w == 3404
tlj inc 465 if ga <= -16
oon inc 494 if ga == -23
blg dec -270 if b >= 766
n dec -816 if xo != -1567
mx inc -519 if oon != 818
ga dec 752 if rt == -771
ty dec -882 if mx < -1432
ga inc -341 if ben < -490
ga dec 680 if bwj != 2807
tlj inc -729 if xo == -1559
rt dec -353 if ben <= -495
tlj inc 949 if ty != 968
k dec -674 if sp > -2131
bwj dec -331 if vlh != 735
ssv inc 736 if bwj < 3135
rt dec 701 if n > -1786
xo dec -284 if ke <= -1115
oon dec 406 if yrf >= 1514
ih inc 144 if ty > 968
oon dec 287 if b != 756
ssv dec 588 if mx == -1434
tlj inc -217 if rt >= -1465
k dec -367 if yrf == 1514
ke dec 475 if ke <= -1114
k inc 572 if jzm == -843
epx inc -878 if vlh != 750
xo inc 889 if bwj >= 3139
n inc -159 if oon <= 139
ty dec 961 if vlh == 745
bwj dec 761 if w != 3404
mlp inc -543 if n != -1936
tlj inc -929 if blg != 410
mx inc 752 if ty <= 12
ke dec -473 if jzm == -836
n inc -250 if ih < 385
ty dec -995 if jzm < -834
jzm inc -367 if epx >= -1100
mlp dec -587 if xo > -1281
xo inc -297 if fdv >= 204
ben dec -791 if blg == 410
ty inc 448 if mx < -678
vlh inc -658 if sp == -2139
blg inc -936 if mx >= -676
xo inc -968 if sp == -2139
rt inc 239 if rt <= -1456
epx inc -910 if mlp != 366
ben inc -509 if vlh == 87
ben dec -936 if b >= 755
tlj dec 410 if bwj > 3122
mx inc -154 if ga >= -1049
bwj inc -470 if ga >= -1046
bwj dec -753 if b > 754
ga dec -150 if tlj <= 709
ssv dec 596 if mx <= -827
ke inc -698 if n != -2191
rt dec 554 if ih <= 386
ga inc 630 if jzm >= -837
ben dec -127 if ty <= 1452
fdv dec 564 if sp < -2137
fdv inc -687 if ih >= 389
mx dec 834 if sp >= -2140
rt inc 503 if w <= 3407
fdv inc -652 if ben != 852
ben dec 981 if rt > -1278
sp inc -396 if bwj < 3421
fdv dec -510 if sp != -2535
w inc 279 if w >= 3399
bwj inc 480 if n != -2199
n dec -430 if ke >= -1821
mx inc -153 if rt > -1282
oon inc -452 if blg <= 414
ke dec 956 if sp < -2525
bwj inc 533 if rt < -1280
ga inc -844 if ssv >= -1222
rt inc -872 if jzm < -841
k dec -439 if ih >= 373
ih dec 315 if n >= -1758
b dec -381 if ke <= -2779
tlj inc 149 if ty <= 1455
vlh dec 672 if k >= -285
ga inc -816 if fdv > -360
mlp inc -187 if ty > 1444
mx inc 265 if ssv <= -1223
tlj inc 229 if ga < -258
w inc 138 if smi <= 1563
bwj dec -707 if xo != -2245
blg dec 555 if vlh > -590
smi dec -930 if smi >= 1570
vlh dec 345 if sp > -2536
ben inc -558 if n >= -1769
ty dec -905 if b == 761
rt inc 98 if ty <= 1447
ga dec -141 if k > -288
oon dec 400 if mx >= -1565
k inc -409 if mx > -1564
tlj dec -370 if smi <= 2493
yrf inc -933 if n > -1752
fdv dec -476 if k <= -688
blg dec -686 if k <= -688
rt inc -121 if smi != 2504
k inc 747 if smi >= 2492
k dec 341 if smi >= 2510
w inc 658 if sp <= -2532
oon inc 890 if ty > 1444
mx inc 790 if tlj != 1087
smi dec 719 if smi <= 2501
n inc -454 if bwj != 4600
n inc 321 if ke == -2772
smi inc 370 if tlj == 1078
ty dec 1 if mx > -1568
w inc -535 if blg <= 550
vlh dec -306 if jzm >= -843
yrf dec 529 if n != -1433
vlh dec 357 if mx != -1558
k dec 392 if ih >= 386
ssv inc -875 if k != 56
w inc -879 if w > 3800
rt dec 927 if ssv <= -1219
smi dec 649 if n == -1439
rt inc 752 if ih < 387
ben inc 612 if fdv == 112
n inc 802 if fdv == 118
tlj dec -175 if oon != 169
k inc -500 if tlj < 1255
k dec 852 if ih != 377
ih dec -107 if fdv <= 110
oon dec -569 if ga != -114
ssv inc 574 if ssv == -1226
blg inc 728 if yrf != 993
yrf inc -921 if vlh <= -619
jzm dec -870 if sp >= -2542
mlp inc 186 if blg < 1279
ih inc 283 if epx <= -2013
bwj dec 890 if ty == 1449
n inc -249 if mx < -1551
smi inc 102 if k != -787
sp inc 682 if bwj > 3706
ty inc 427 if ih != 661
fdv dec -721 if xo != -2243
rt dec 886 if ga <= -131
ty inc 463 if ke <= -2771
vlh dec -61 if bwj == 3710
rt inc 773 if ben >= -690
k inc -659 if mlp != 369
ssv inc 562 if b == 762
ssv inc 584 if ke < -2767
ke inc 989 if smi <= 1243
ty inc 319 if ke > -1793
xo inc 540 if oon <= 746
rt dec 763 if sp >= -1859
smi inc -363 if b >= 759
vlh dec -740 if w < 2933
ben inc -590 if rt != -1571
ty inc 349 if sp < -1848
mlp inc -330 if oon <= 731
yrf dec 389 if vlh == 177
smi dec -983 if ben != -1275
xo dec 282 if w > 2929
bwj dec -605 if oon != 749
k inc 433 if blg != 1272
sp inc -504 if ben == -1277
ke dec 274 if mx >= -1560
mlp inc -638 if w == 2927
mx inc -15 if smi > 1860
w inc -91 if ty > 3002
rt dec 64 if tlj < 1264
smi inc 994 if ben > -1274
w dec -442 if bwj >= 4310
ty dec -777 if ty <= 3014
jzm inc -394 if epx >= -2026
fdv inc -909 if epx == -2018
smi dec 255 if sp <= -2355
vlh inc 591 if fdv <= -795
ben inc 384 if vlh >= 761
tlj inc -479 if yrf == -330
ssv inc -179 if epx == -2018
ke dec 984 if fdv < -791
ben inc 570 if b > 768
ty dec -231 if mx < -1557
vlh dec 844 if rt != -1616
smi inc 932 if rt == -1626
ih dec -609 if fdv == -795
ga inc -774 if bwj == 4315
fdv inc 640 if bwj == 4315
w inc 132 if n >= -1697
ssv inc -957 if rt == -1626
ty dec -205 if fdv <= -153
fdv inc 32 if bwj <= 4320
vlh dec 960 if vlh >= -82
tlj dec 658 if oon == 740
k inc 654 if ke >= -3033
vlh dec -562 if vlh > -1043
sp inc 308 if w > 3405
oon inc -795 if smi >= 2522
ssv dec -728 if epx == -2018
sp dec 843 if bwj != 4325
k inc -745 if ga >= -891
n inc -767 if yrf < -329
vlh inc 323 if mx < -1554
ga inc 513 if bwj == 4315
xo inc -419 if ga != -384
b dec 5 if ga != -374
ga inc -651 if xo > -1705
k inc 266 if ke > -3042
tlj inc 172 if b > 757
ke dec -916 if smi >= 2539
w inc 262 if n <= -1681
n dec -154 if ssv < 80
yrf inc 243 if ga < -1044
rt dec 523 if b < 757
ben inc 383 if sp < -2882
k inc -987 if vlh < -149
oon inc 676 if ben <= -508
mlp inc 782 if ga < -1033
fdv inc 687 if jzm > -353
rt dec -746 if n == -1688
w dec -358 if smi <= 2537
jzm inc -365 if k > -1745
smi dec 978 if rt != -888
tlj inc 294 if mx >= -1562
yrf inc 215 if vlh == -144
smi dec 732 if xo < -1693
rt inc -533 if yrf > -332
sp dec -348 if ga <= -1030
ga inc -374 if ih < 1282
ga inc -362 if ke < -3048
b inc -404 if ty > 4214
blg inc -702 if ke <= -3047
rt dec -868 if ben != -510
ben inc 412 if blg >= 1268
ssv dec 888 if ssv <= 81
w inc 850 if tlj != 898
w inc 697 if oon > 619
ih inc 885 if w >= 4718
yrf dec 778 if k != -1749
b dec -155 if b > 344
epx dec -49 if ga <= -1403
ty dec 683 if ih == 2162
n inc 127 if blg != 1267
w dec -640 if ty < 4229
ty dec 798 if blg == 1269
blg dec 197 if k != -1739
ga inc 803 if ben == -100
mlp dec 230 if bwj != 4315
mx inc -560 if smi >= 818
ga inc 629 if fdv > -128
smi dec -585 if ben >= -106
vlh dec -145 if epx >= -1976
ty inc 654 if tlj >= 908
yrf dec 775 if bwj < 4319
sp inc 364 if oon > 630
ke inc 152 if ga >= -774
ke inc 430 if ih == 2157
epx inc -137 if k == -1743
w dec -429 if k != -1738
smi dec 306 if n >= -1561
ga inc -495 if ke < -2606
w dec 364 if mlp != 511
smi dec -994 if n >= -1570
epx dec 440 if ssv >= 84
epx inc 197 if oon == 623
sp dec 378 if ssv != 81
jzm inc 633 if w != 5432
oon dec 824 if blg < 1074
tlj dec 927 if ty <= 3425
b dec -240 if xo < -1697
yrf inc 916 if rt <= -1413
mlp inc 51 if oon != -212
xo inc -509 if b < 754
mx inc 498 if bwj != 4319
ty inc -682 if ben != -91
oon inc -498 if mlp == 559
bwj dec -46 if ih < 2155
bwj dec -133 if ga != -1278
smi dec 678 if oon < -700
ih inc -775 if mx < -1614
mx inc -312 if fdv != -114
xo dec -931 if fdv > -133
sp dec 623 if ben <= -96
blg dec -602 if epx <= -2541
vlh inc 709 if ssv != 86
k inc 23 if yrf >= -953
rt inc 955 if fdv > -127
blg inc -971 if jzm < -719
ty dec 500 if blg <= 707
rt inc 905 if epx < -2550
w inc -648 if b == 748
n inc 417 if vlh > -1
sp inc 132 if ben == -98
ben inc -18 if fdv <= -119
xo inc 911 if n < -1552
vlh dec -719 if n < -1555
fdv dec -284 if vlh < 719
ben dec -849 if smi == 1426
ty inc -188 if w > 4781
epx dec -569 if blg == 703
mx inc 128 if epx >= -1985
ty inc 929 if ty != 2048
ga dec -602 if mlp == 559
yrf inc 280 if ty < 2984
w inc 180 if rt == -458
ben dec -664 if xo == -370
mlp dec -495 if fdv <= 165
ben dec -292 if w <= 4965
xo dec 335 if tlj < -24
ga inc -778 if ssv == 86
vlh inc -915 if ty == 2981
sp inc 665 if vlh < -208
jzm dec 961 if smi > 1415
b dec -160 if ke > -2612
ga dec 665 if n != -1565
xo dec 813 if ssv == 93
epx dec 642 if mx != -1805
ben dec 199 if ke >= -2611
ke dec -517 if blg != 710
ga inc -788 if smi != 1408
ben inc -201 if bwj != 4450
rt inc 893 if smi > 1411
n inc -659 if blg == 703
blg dec -92 if bwj <= 4449
k dec -949 if bwj == 4448
ssv inc -540 if ssv != 93
tlj inc 116 if b == 908
rt dec -587 if ty >= 2977
sp inc 94 if jzm != -1681
oon dec 985 if ga >= -2904
n inc -2 if blg <= 802
epx dec -263 if blg < 802
ih dec -394 if ty != 2978
oon dec 979 if b >= 901
tlj inc 687 if vlh < -198
ty dec 152 if fdv != 153
oon dec 848 if ben == 440
ty dec -596 if blg <= 792
fdv dec -391 if tlj != 783
n dec -148 if ty < 2839
ke inc -333 if w <= 4964
mlp inc -174 if ga <= -2895
rt inc 11 if tlj != 784
ty dec 610 if ben < 444
ben inc -901 if oon != -3513
sp dec 484 if blg >= 793
mlp dec 131 if epx == -2356
mx dec -973 if yrf < -675
n inc -455 if ih != 1779
blg inc 696 if sp != -3793
k inc -780 if w < 4967
b inc 783 if jzm >= -1680
sp dec -17 if oon < -3509
b inc 418 if b != 902
jzm dec -116 if ga != -2895
ben dec -26 if epx > -2355
jzm inc -116 if blg < 1498
smi inc -137 if mlp == 739
n inc 521 if epx != -2353
vlh inc 889 if ty == 2219
epx inc -454 if yrf == -682
tlj dec -154 if vlh < 680
n dec 585 if oon <= -3508
xo inc 401 if mx > -824
ke dec 368 if mlp != 739
blg dec -737 if mlp <= 751
fdv dec -935 if mlp <= 741
tlj dec -666 if sp != -3788
rt dec -519 if w >= 4960
smi dec -598 if epx == -2810
ben dec -16 if ke >= -2789
rt dec -844 if yrf < -680
oon dec -945 if yrf < -673
sp dec -101 if vlh > 685
xo inc -737 if ke >= -2797
fdv inc 346 if smi == 2014
ssv dec -261 if tlj != 1441
ga dec -176 if sp != -3685
fdv dec 737 if mx > -838
smi inc 842 if k >= -1581
blg inc -939 if bwj <= 4452
rt inc 415 if xo >= -1448
b dec -207 if xo > -1445
tlj dec -923 if jzm <= -1678
yrf dec -814 if ssv > -196
jzm dec 845 if sp > -3688
rt inc 594 if b < 1534
ty inc 185 if mlp < 757
b inc 220 if xo >= -1448
mx inc 464 if w < 4971
fdv dec -627 if n >= -2590
tlj inc 904 if bwj == 4448
n dec -37 if mlp > 757
fdv inc -157 if k <= -1568
rt inc 709 if ty >= 2400
mx dec -716 if epx <= -2807
sp inc -861 if vlh != 697
mlp dec -812 if ssv < -188
ke inc 95 if jzm < -2532
k inc -115 if ssv >= -197
mlp dec 489 if smi <= 2859
mx dec -424 if ty <= 2396
ty inc 474 if sp == -4543
blg dec -161 if epx > -2820
b inc 23 if n == -2593
bwj inc -119 if fdv == 4
fdv dec 210 if rt <= 4118
k dec -539 if smi == 2856
jzm dec 592 if mlp < 1079
tlj inc 886 if vlh < 680
oon dec 940 if k >= -1154
blg dec 426 if k < -1147
k dec 438 if ssv < -192
ssv inc -937 if ih >= 1776
epx inc 340 if ben <= 443
n dec 267 if fdv > -215
w dec -280 if jzm < -3113
oon dec 564 if epx > -2471
fdv dec -497 if oon > -4067
smi dec -22 if xo == -1442
tlj dec 592 if b >= 1772
jzm dec -637 if yrf <= 127
jzm inc -595 if ben < 438
k dec -75 if bwj > 4322
yrf inc -504 if b >= 1769
n dec 485 if yrf < -370
ben inc -379 if mx == 349
bwj inc -492 if epx <= -2473
ty dec 669 if blg >= 1019
bwj inc -607 if k > -1523
n dec 566 if ih <= 1784
rt dec 151 if ssv == -1134
epx dec 79 if vlh <= 687
mx inc 403 if xo != -1435
blg inc -458 if jzm == -3123
mx inc -850 if epx != -2546
ben dec -82 if blg == 560
ih inc -480 if mx <= -103
ty dec -197 if bwj > 3724
ben dec -252 if xo <= -1447
blg inc 522 if blg < 568
ke dec -376 if ih > 1771
b dec -354 if ben <= 66
sp dec 156 if w != 5241
n inc -140 if w != 5249
bwj dec -732 if ty >= 1737
oon inc 432 if epx < -2543
bwj dec 968 if fdv >= -208
mlp dec -879 if ih > 1767
xo inc 276 if jzm < -3119
oon dec -680 if blg >= 1088
vlh dec 623 if sp == -4710
ih dec -791 if xo <= -1165
fdv dec 52 if rt != 4120
oon dec 969 if n > -4042
oon dec -711 if jzm > -3129
ben inc -899 if vlh >= 679
fdv inc 604 if n > -4060
ssv inc 11 if jzm <= -3122
fdv dec -591 if smi <= 2884
mx inc -563 if w < 5252
tlj inc 518 if mx != -667
n inc 16 if ke == -2415
vlh inc 536 if w == 5244
ga dec -7 if rt == 4114
smi dec 349 if epx < -2556
k inc -719 if b < 2136
oon dec -522 if smi < 2886
ih inc 420 if ssv < -1109
w dec -812 if ssv == -1119
blg dec -731 if xo != -1173
vlh inc 670 if ssv != -1111
b inc 610 if k != -2227
ih inc 111 if fdv != 934
blg inc -579 if xo != -1163
rt inc -632 if w <= 6058
oon inc -124 if vlh != 1902
w dec 638 if bwj == 2754
bwj inc -192 if xo == -1166
tlj inc 252 if fdv < 942
jzm inc 96 if yrf <= -365
epx inc 42 if vlh >= 1891
n inc 580 if sp <= -4696
rt dec 264 if rt == 3482
b dec -219 if ssv < -1122
blg inc -307 if mx < -663
ke inc 613 if tlj > 3437
fdv inc 142 if k <= -2236
mx dec 548 if rt != 3224
jzm inc 580 if jzm >= -3036
tlj dec -910 if bwj == 2562
mlp inc -517 if vlh < 1899
ke inc 352 if ty == 1735
mlp inc -632 if rt == 3218
xo inc 766 if smi <= 2884
epx dec 912 if vlh >= 1896
epx inc 616 if mlp == 803
sp dec 233 if w >= 5417
xo inc 940 if ben <= -847
ih inc -398 if tlj >= 4355
n dec -660 if k <= -2224
w dec 697 if w <= 5410
ssv inc -26 if n < -2816
n inc 656 if oon == -1851
bwj dec 560 if xo < -397
ty dec -13 if mlp != 809
mlp dec 47 if blg < 1248
yrf dec -992 if ty == 1748
mx inc -563 if vlh <= 1901
oon dec 212 if fdv > 929
fdv inc 125 if smi < 2880
epx inc -256 if w <= 5421
yrf inc 998 if fdv <= 1068
smi dec -463 if ssv > -1129
smi inc 776 if oon <= -2064
mlp inc -334 if mx != -1770
tlj inc 420 if ty != 1756
yrf inc -801 if w > 5417
ty inc -561 if epx <= -2754
blg dec 104 if blg == 1240
fdv dec 772 if k > -2242
oon dec 885 if rt == 3218
sp dec 372 if rt == 3218
n dec 165 if ke <= -1446
smi dec -175 if xo <= -392
ke dec 524 if tlj >= 4768
vlh inc -101 if k > -2224
k inc 86 if ben < -833
bwj dec 622 if jzm < -2443
ke inc -437 if tlj <= 4781
tlj inc 287 if oon >= -2943
jzm dec -85 if vlh > 1886
xo inc 451 if blg != 1129
bwj inc 51 if vlh < 1896
blg dec 545 if ssv < -1114
blg dec 628 if ke != -2421
xo inc -592 if tlj == 4775
xo dec 799 if fdv >= 281
blg inc 250 if ssv < -1117
yrf inc -521 if w == 5418
ssv inc 494 if blg <= 222
ben inc -188 if mx <= -1773
sp dec -315 if mlp >= 421
ssv inc 995 if mx > -1777
ih dec -375 if n > -2324
epx inc 340 if ke >= -2416
mx inc 281 if fdv >= 295
blg inc 411 if sp < -4982
vlh inc -558 if sp < -4987
vlh inc 901 if jzm < -2353
sp dec -823 if epx < -2429
ga dec -945 if yrf != 296
k inc -144 if ty == 1187
yrf inc -907 if tlj != 4779
n dec 592 if k <= -2282
k dec 80 if sp != -4990
smi dec -914 if fdv == 290
b inc 690 if k >= -2362
epx inc 113 if ssv == 370
yrf inc -45 if blg <= 626
bwj dec -157 if ke < -2419
w inc -255 if ga != -2902
smi inc -481 if yrf != -647
xo dec 416 if ke == -2415
ih inc -18 if xo == -1756
blg dec -507 if w != 5167
tlj inc 865 if mlp != 419
w dec 660 if vlh < 2243
mlp inc 13 if fdv != 295
vlh dec 713 if mx < -1771
ke dec 537 if rt == 3208
mx dec -454 if ke >= -2410
mlp dec -858 if ty == 1187
yrf inc 655 if smi <= 3950
rt dec -215 if ty != 1187
sp dec 974 if fdv <= 295
b inc -347 if rt <= 3215
ssv dec -805 if ben < -830
ben inc 696 if n <= -2904
ke inc -141 if ty == 1195
oon inc -379 if ty <= 1195
bwj dec -798 if oon >= -3332
blg dec -888 if bwj >= 2235
ty inc -7 if rt != 3223
sp dec -651 if ssv < 1179
sp inc -104 if epx >= -2313
n dec -463 if ty > 1175
fdv inc -176 if jzm > -2369
tlj dec 395 if sp > -5422
tlj inc -382 if n <= -2447
n dec -309 if yrf > -4
ga inc 653 if mx > -1776
sp dec 774 if ssv >= 1184
smi dec -567 if mlp > 1290
ty dec 494 if xo <= -1753
fdv inc -734 if ga >= -2245
tlj dec 135 if bwj < 2232
n inc -536 if ih != 3047
mx dec -34 if b < 2748
epx inc 238 if oon == -3327
mlp dec 773 if w != 4510
smi inc 586 if tlj == 4728
sp inc -201 if n > -2683
w dec 463 if n != -2685
xo dec -487 if n == -2685
ssv inc -846 if yrf > -7
vlh inc -554 if vlh == 1523
oon inc 119 if rt != 3218
blg dec -243 if mlp <= 509
smi dec 526 if k == -2376
tlj dec -170 if blg < 1128
oon inc -371 if mx > -1741
jzm dec -129 if jzm > -2357
ih inc 573 if w > 4039
jzm inc -832 if ty >= 693
sp inc 419 if mx >= -1739
ssv inc 804 if sp <= -5201
yrf dec 310 if tlj >= 4736
tlj dec -274 if fdv >= -620
bwj inc -486 if yrf >= -10
blg inc 138 if xo <= -1761
n inc -940 if epx != -2071
xo inc -972 if tlj >= 4999
rt inc 522 if jzm < -2357
ga inc -397 if b != 2737
ty inc 799 if ssv < 1140
ih dec -938 if epx < -2079
n dec -590 if k > -2369
tlj dec 865 if sp >= -5201
ty inc -725 if oon < -3693
w dec -878 if xo != -2719
ih dec 618 if w > 4919
k inc -180 if bwj == 1743
xo inc -821 if ga > -2643
epx dec -892 if epx >= -2073
sp inc 428 if smi >= 5099
yrf dec 176 if n < -3609
xo inc -936 if tlj <= 4139
w inc -542 if fdv == -620
smi dec -714 if fdv != -624
ih dec -843 if ke < -2406
b dec 558 if yrf == -177
n dec -523 if sp >= -4781
n inc 938 if mx >= -1742
epx inc 973 if n > -2148
jzm inc 150 if fdv != -620
ih inc 673 if tlj != 4146
tlj inc -772 if n <= -2152
blg dec -155 if xo == -4485
epx dec 498 if oon >= -3695
ke inc -393 if oon > -3700
ssv dec -129 if oon > -3700
vlh inc 63 if mx >= -1740
ben inc -257 if xo <= -4487
rt dec 297 if oon == -3698
ty inc 347 if b <= 2188
ben inc -961 if xo < -4484
ih inc -624 if ssv < 1271
tlj inc 31 if k > -2557
epx inc -546 if bwj > 1734
blg dec 580 if fdv <= -615
fdv dec -170 if mx <= -1729
yrf inc 475 if ssv > 1261
vlh dec 822 if b == 2182
ty inc -414 if blg == 706
mx dec -871 if ih < 4518
ben dec 908 if mlp == 519
b inc 50 if ke >= -2809
mlp inc 904 if ty < 697
blg inc -465 if vlh != 209
b inc -71 if k < -2543
mx dec -420 if epx == -1726
ty dec -193 if jzm >= -2370
vlh dec -526 if ke < -2814
ben inc -121 if jzm != -2362
ga dec -892 if ih == 4522
vlh dec -607 if vlh != 210
bwj inc 569 if xo < -4487
tlj dec 514 if tlj < 3395
smi dec -221 if ih <= 4522
ssv inc 621 if b < 2171
k inc -185 if rt != 3439
blg dec -42 if ssv >= 1890
ssv inc 24 if ty > 891
yrf inc 572 if n < -2153
ben inc 920 if mlp == 1423
b inc -699 if mx > -1325
blg dec 952 if k != -2732
b dec 96 if blg != -708
vlh inc -200 if sp != -4763
xo inc -741 if epx > -1731
ga inc 944 if w >= 4370
oon inc 652 if rt != 3451
mlp dec -710 if ty > 883
vlh dec -121 if ke >= -2801
bwj inc -490 if smi >= 6032
xo dec -678 if xo == -5226
epx inc -796 if oon != -3039
ssv inc -339 if xo < -4547
smi inc -30 if rt > 3440
b inc 476 if ssv != 1553
xo inc 155 if smi < 6014
k dec -79 if sp <= -4766
mlp inc 592 if k != -2656
epx dec 74 if rt < 3445
tlj dec -575 if n >= -2149
vlh inc 34 if mlp == 2133
ty dec -648 if epx >= -2604
mx inc 146 if fdv >= -454
mx dec -212 if ke < -2812
vlh inc 635 if smi < 6008
k inc 7 if sp != -4778
oon inc 832 if fdv <= -447
rt inc -936 if ih >= 4518
b inc -236 if blg == -711
oon inc -650 if oon > -2221
xo inc -397 if vlh == 679
ih inc -248 if sp >= -4777
rt inc -904 if k == -2649
mx inc -222 if sp != -4780
yrf inc -793 if jzm >= -2362
bwj dec -186 if oon < -2861
n inc 812 if epx <= -2589
w dec 315 if vlh != 682
blg inc 840 if tlj > 3391
mlp inc -203 if epx < -2591
mx inc -959 if ben <= -1090
jzm inc 360 if ke > -2812
xo inc -131 if n >= -1348
tlj dec -618 if jzm < -1997
blg dec 440 if b > 1598
oon dec 293 if ke < -2802
ih dec 391 if ih == 4274
ke dec -141 if ssv > 1546
epx dec 995 if vlh > 672
yrf inc 860 if ga < -808
jzm dec 594 if bwj != 1443
fdv inc 172 if sp > -4775
sp dec -473 if xo <= -4922
ga inc -813 if jzm < -2601
epx inc -554 if fdv == -278
jzm dec -716 if bwj >= 1433
b dec 523 if sp == -4773
vlh dec 474 if oon > -3161
tlj dec 219 if ga == -805
blg inc -824 if ga >= -810
bwj dec 266 if yrf != 68
epx dec -138 if ben <= -1083
oon dec 627 if yrf > 76
ben dec -908 if bwj <= 1179
epx dec -121 if n > -1352
ssv dec -173 if rt < 1604
rt dec -458 if ty != 1524
yrf dec 236 if epx != -3888
mx dec -391 if jzm >= -1881
bwj dec 534 if w > 4052
b inc -959 if tlj < 3798
ty dec -868 if vlh != 211
jzm inc 728 if smi == 6007
yrf dec -584 if ty > 2399
smi dec 905 if yrf <= 434
vlh inc 455 if ssv != 1717
fdv dec 959 if jzm >= -1160
jzm dec -870 if rt <= 2069
k inc -635 if rt > 2057
b dec 710 if vlh > 198
blg inc -456 if ben <= -174
ga inc 580 if sp != -4774
bwj inc 762 if ssv < 1722
mx inc 405 if jzm == -282
ben inc -838 if n > -1350
ben inc 360 if sp >= -4777
ke inc 164 if rt < 2068
mx dec 891 if b != -595
jzm dec -395 if w != 4052
k dec -690 if xo < -4920
ga inc -272 if ty < 2406
xo inc 131 if b != -595
k inc -214 if vlh > 201
ssv dec 122 if vlh == 205
ga dec 603 if blg < -1588
vlh inc 770 if oon > -3789
epx dec -187 if ga > -1101
oon inc -128 if yrf > 421
ty dec -781 if oon >= -3916
ben dec -334 if epx != -3694
fdv inc 137 if ben >= -330
vlh dec 92 if fdv < -1098
jzm inc -413 if ga <= -1097
vlh dec 758 if sp < -4764
tlj inc -754 if xo > -4788
jzm dec 79 if mx >= -2449
bwj dec 983 if w != 4067
fdv inc -819 if b != -586
epx inc 615 if jzm >= -373
mlp inc -871 if fdv >= -1108
ih inc -945 if b < -576
mx dec -934 if w == 4068
"""
