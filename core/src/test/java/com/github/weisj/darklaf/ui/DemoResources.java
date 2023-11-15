/*
 * MIT License
 *
 * Copyright (c) 2019-2023 Jannis Weis
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */
package com.github.weisj.darklaf.ui;

import javax.swing.*;

import com.github.weisj.darklaf.iconset.AllIcons;

public final class DemoResources {
    public static final String LOREM_IPSUM =
            """
                    Lorem ipsum dolor sit amet, consectetur adipiscing elit. In tempor quis nibh a semper. Nullam
                     auctor, erat non viverra commodo, libero orci aliquam quam, ac interdum nunc est sed
                     ligula. Aliquam vel velit non dolor accumsan blandit id eu metus. Aenean iaculis urna in
                     placerat aliquam. Aliquam dui quam, bibendum sed magna in, cursus ornare est. Quisque
                     tempor nunc quis nunc tempor convallis. Vestibulum tristique luctus ante, ac hendrerit dui.

                    Donec ut maximus augue. Nam eleifend maximus scelerisque. Duis varius accumsan est, non
                     aliquam dolor. Aenean iaculis nibh in aliquam viverra. Sed laoreet, urna ut facilisis
                     convallis, arcu turpis vestibulum augue, id convallis tellus metus nec orci. Lorem ipsum
                     dolor sit amet, consectetur adipiscing elit. Donec hendrerit purus velit, at blandit elit
                     luctus ut. Proin diam nisl, sodales vitae dignissim nec, eleifend eu libero. Maecenas odio
                     ligula, fermentum eget nisl vel, cursus tristique est. In nec nibh nec dui tempor
                     ullamcorper. Praesent tincidunt luctus sem, ut luctus dolor commodo non. Nulla consectetur
                     facilisis dolor, in facilisis ligula fringilla et. Cras id placerat libero. Donec
                     vehicula orci a quam rutrum, eu efficitur lorem iaculis. Aenean varius nisi in dictum
                     accumsan.

                    Nulla massa ipsum, consectetur non gravida ut, blandit quis velit. Ut pretium quam aliquam
                     diam porttitor mattis. Nam ullamcorper, felis ut iaculis iaculis, nunc odio pulvinar\s
                    enim, vitae iaculis turpis sapien iaculis metus. Donec rutrum varius augue in dictum. Cras
                     vestibulum vitae mauris ut finibus. Ut dictum imperdiet lorem et imperdiet. Vivamus\s
                    semper tempor dolor eu porta. Sed at vehicula nisl. Pellentesque ut lorem tincidunt,\s
                    elementum ligula at, porta turpis. Praesent feugiat dolor diam, at facilisis metus gravida
                     non. Aliquam quis pellentesque nibh. Sed vestibulum porttitor nisi. In vitae malesuada
                     sapien.""";
    public static final String HTML_DEMO = """
            <h1>Lorem ipsum dolor sit amet consectetuer adipiscing\s
            elit</h1>


            <p>Lorem ipsum dolor sit amet, consectetuer adipiscing\s
            elit. Aenean commodo ligula eget dolor. Aenean massa\s
            <strong>strong</strong>. Cum sociis natoque penatibus\s
            et magnis dis parturient montes, nascetur ridiculus\s
            mus. Donec quam felis, ultricies nec, pellentesque\s
            eu, pretium quis, sem. Nulla consequat massa quis\s
            enim. Donec pede justo, fringilla vel, aliquet nec,\s
            vulputate eget, arcu. In enim justo, rhoncus ut,\s
            imperdiet a, venenatis vitae, justo. Nullam dictum\s
            felis eu pede <a class="external ext" href="#">link</a>\s
            mollis pretium. Integer tincidunt. Cras dapibus.\s
            Vivamus elementum semper nisi. Aenean vulputate\s
            eleifend tellus. Aenean leo ligula, porttitor eu,\s
            consequat vitae, eleifend ac, enim. Aliquam lorem ante,\s
            dapibus in, viverra quis, feugiat a, tellus. Phasellus\s
            viverra nulla ut metus varius laoreet. Quisque rutrum.\s
            Aenean imperdiet. Etiam ultricies nisi vel augue.\s
            Curabitur ullamcorper ultricies nisi.</p>


            <h1>Lorem ipsum dolor sit amet consectetuer adipiscing\s
            elit</h1>


            <h2>Aenean commodo ligula eget dolor aenean massa</h2>


            <p>Lorem ipsum dolor sit amet, consectetuer adipiscing\s
            elit. Aenean commodo ligula eget dolor. Aenean massa.\s
            Cum sociis natoque penatibus et magnis dis parturient\s
            montes, nascetur ridiculus mus. Donec quam felis,\s
            ultricies nec, pellentesque eu, pretium quis, sem.</p>


            <h2>Aenean commodo ligula eget dolor aenean massa</h2>


            <p>Lorem ipsum dolor sit amet, consectetuer adipiscing\s
            elit. Aenean commodo ligula eget dolor. Aenean massa.\s
            Cum sociis natoque penatibus et magnis dis parturient\s
            montes, nascetur ridiculus mus. Donec quam felis,\s
            ultricies nec, pellentesque eu, pretium quis, sem.</p>


            <ul>
              <li>Lorem ipsum dolor sit amet consectetuer.</li>
              <li>Aenean commodo ligula eget dolor.</li>
              <li>Aenean massa cum sociis natoque penatibus.</li>
            </ul>

            hr:<hr></hr>
            custom div:<div style="height: 0px; border-top-style: solid; border-width: 1px"></div>
            <p>Lorem ipsum dolor sit amet, consectetuer adipiscing\s
            elit. Aenean commodo ligula eget dolor. Aenean massa.\s
            Cum sociis natoque penatibus et magnis dis parturient\s
            montes, nascetur ridiculus mus. Donec quam felis,\s
            ultricies nec, pellentesque eu, pretium quis, sem.</p>


            <form action="#" method="post">
              <fieldset>
                <label for="name">Name:</label>
                <input type="text" id="name" placeholder="Enter your\s
            full name" />

                <label for="email">Email:</label>
                <input type="email" id="email" placeholder="Enter\s
            your email address" />

                <label for="message">Message:</label>
                <textarea id="message" placeholder="What's on your\s
            mind?"></textarea>

                <input type="submit" value="Send message" />

              </fieldset>
            </form>


            <p>Lorem ipsum dolor sit amet, consectetuer adipiscing\s
            elit. Aenean commodo ligula eget dolor. Aenean massa.\s
            Cum sociis natoque penatibus et magnis dis parturient\s
            montes, nascetur ridiculus mus. Donec quam felis,\s
            ultricies nec, pellentesque eu, pretium quis, sem.</p>


            <table class="data">
              <tr>
                <th>Entry Header 1</th>
                <th>Entry Header 2</th>
                <th>Entry Header 3</th>
                <th>Entry Header 4</th>
              </tr>
              <tr>
                <td>Entry First Line 1</td>
                <td>Entry First Line 2</td>
                <td>Entry First Line 3</td>
                <td>Entry First Line 4</td>
              </tr>
              <tr>
                <td>Entry Line 1</td>
                <td>Entry Line 2</td>
                <td>Entry Line 3</td>
                <td>Entry Line 4</td>
              </tr>
              <tr>
                <td>Entry Last Line 1</td>
                <td>Entry Last Line 2</td>
                <td>Entry Last Line 3</td>
                <td>Entry Last Line 4</td>
              </tr>
            </table>


            <p>Lorem ipsum dolor sit amet, consectetuer adipiscing\s
            elit. Aenean commodo ligula eget dolor. Aenean massa.\s
            Cum sociis natoque penatibus et magnis dis parturient\s
            montes, nascetur ridiculus mus. Donec quam felis,\s
            ultricies nec, pellentesque eu, pretium quis, sem.</p>
            """;
    public static final String KERNING_TEST = """
            Basic Kerning Text


            BASIC ALPHABET\s

            AÆBCÇDEFGH
            IJKLMNOŒØP
            QRSTUVWXYZ/\\
            aæbcçdefﬁﬂghijklm
            noœøpqrstuvwxyz
            1234567890
            °’”$¢ƒß£¥%⁄#
            &.,:;…·!?“”‘’
            •- – —*„‚_|®©™
            ([{«»‹›}])
            ˆ˜¯˘˙˚¸˝˛ˇ`´¨
            †‡§¶@+=±


            FOREIGN CHARS

            ÄÅÂÁÀÃÇÉÊËÈÍÎÏÌ\s
            ÑÖÓÔÒÕÜÚÛÙŸ
            áàâäãåçéèêëíìîïñ
            óòôöõúùûüÿ


            Pi & OTHER/TEXT FONTS

            _^|∏πΩ¬ªº¿¡
            ∆◊§∞µ∂∑€‰


            SAMPLE KERNING PAIRS

            AT AV AW AY Av Aw Ay\s
            Fa Fe Fo Kv Kw Ky LO\s
            LV LY PA Pa Pe Po TA\s
            Ta Te Ti To Tr Ts Tu Ty\s
            UA VA Va Ve Vo Vr Vu Vy\s
            WA WO Wa We Wr Wv Wy\s


            SAMPLE LOWERCASE

            INCIDENTALS

            w! w? f! f? ¡a ¿a
            «n»«o»‹n›‹o›
            h∂ho&o‚{h}h•h
            o•oh@hªhº
            h⁄h‚h„h™h®h©h
            h·h o·o`´˘¨ˆ˜¯ˇ˙˚¸˝˛ˆ
            h”h“h“w”\s

            h.h,h:h;‘h’
            o.o,o:o;‘o’
            (h)[h]h/h\\h
            (o)[o]h-ho-o
            (h)[h]h/h\\h#
            (o)[o]h-ho-h
            h£h$h¢hƒh¥h
            h*h†h‡h§
            h–ho–o
            h—ho—o
            h+h=h±h'h"h°h#h
            h%h‰h…h
            h1h2h3h4h5h0h
            h6h7h8h9h0h
            £110121314151
            $16171819122
            23242526¢
            27282930%


            SAMPLE SIDEBARING

            HAHBHCHDHEHFHGHHH
            HIHJHKHLHMHNHOHPHQH
            HRHSHTHUHVHWHXHYHZH
            HÆHŒHÄHÖHÜHÅHÇHÁH
            HÛHÍHÏHÌHÓEHÙHØH&HaH
            HbHcHdHeHfHgHhHiHjHvkH
            HlHmHnHoHpHqHrHsHtHuH
            HvHwHxHyHzHßHæHœHäH
            HöHüHﬁHﬂHåHıHçHøH1H2H
            H3H4H5H6H7H8H9H0H£H¢H
            H$H¥HƒH!H¡H?H¿H*H#H/H+H
            H=H÷H≠H±HvH∂H◊H≈H√H~HµH
            H∞H.H,H;H:H”H”H”H‘H’H…H
            H‹H›H»H«H[H]H(H)H{H}H⁄H–H
            H‰H%H¶H|H’H™H®H©H•H@H
            H˘HˆH`H´HˇH˜H¨H·H¸H¯H˚H˛H˝H˙H


            SAMPLE TEXT

            Think about it, your unique designs\s
            evaluated by experts in typography,\s
            fabulous prizes, recognition and the\s
            envy of your peers! Not only that, your\s
            work will be offered for sale around\s
            the world by the best alternative type\s
            foundry going. And that would be\s
            us...GarageFonts.


            COMPARISONS

            ÷±≤≥–—≠<=>-+≈
            ([{«»‹›}])‰%¡¿!?
            “”‘’°’”ˆ˜¯˘˙˚˝ˇ`´¨„‚.,:;…·
            ®©™@


            ALL CAPS

            AABACADAEAFAGAHAIAJAK
            ALAMANAOAPAQARASATAU
            AVAWAXAYAZA A’S A”
            A-A–A—A…A. A, A; A: A? A!
            BABBCBDBEBFBGBHBIBJBK
            BLBMBNBOBPBQBRBSBTBU
            BVBWBXBYBZB B’S B”
            B-B–B—B…B. B, B; B: B? B!
            CACBCCDCECFCGCHCICJCK
            CLCMCNCOCPCQCRCSCTCU
            CVCWCXCYCZC C’S C”
            C-C–C—C…C. C, C; C: C? C!
            DADBDCDDEDFDGDHDIDJDK
            DLDMDNDODPDQDRDSDTDU
            DVDWDXDYDZD D’S D”DØD
            D-D–D—D…D. D, D; D: D? D!
            EAEBECEDEEFEGEHEIEJEK
            ELEMENEOEPEQERESETEU
            EVEWEXEYEZE E’S E”
            E-E–E—E…E. E, E; E: E? E!
            FAFBFCFDFEFFGFHFIFJFK
            FLFMFNFOFPFQFRFSFTFU
            FVFWFXFYFZF F’S F”
            F-F–F—F…F. F, F; F: F? F!
            GAGBGCGDGEGFGGHGIGJGK
            GLGMGNGOGPGQGRGSGTGU
            GVGWGXGYGZG G’S G”
            G-G–G—G…G. G, G; G: G? G!
            HAHBHCHDHEHFHGHHIHJHK
            HLHMHNHOHPHQHRHSHTHU
            HVHWHXHYHZH H’S H”
            H-H–H—H…H. H, H; H: H? H!
            IAIBICIDIEIFIGIHIIJIK
            ILIMINIOIPIQIRISITIUIVIWIXIYIZ
            I’S I”I-I–I—I…I. I, I; I: I? I!
            JAJBJCJDJEJFJGJHJIJJK
            JLJMJNJOJPJQJRJSJTJU
            JVJWJXJYJZJ J’S J”
            J-J–J—J…J. J, J; J: J? J!
            KAKBKCKDKEKFKGKHKIKJK
            KLKMKNKOKPKQKRKSKTKU
            KVKWKXKYKZK K’S K”
            K-K–K—K…K. K, K; K: K? K!
            LALBLCLDLELFLGLHLILJLK
            LLMLNLOLPLQLRLSLTLU
            LVLWLXLYLZL L’S L”
            L-L–L—L…L. L, L; L: L?L
            L!MAMBMCMDMEMFMGMHMIMJMK
            MLMMNMOMPMQMRMSMTMU
            MVMWMXMYMZM M’S M”
            M-M–M—M…M. M, M; M: M? M!
            NANBNCNDNENFNGNHNINJNK
            NLNMNNNONPNQNRNSNTNU
            NVNWNXNYNZN N’S N”
            N-N–N—N…N. N, N; N: N? N!
            OAOBOCODOEOFOGOHOIOJOK
            OLOMONOOPOQOROSOTOU
            OVOWOXOYOZO O’S O”
            O-O–O—O…O. O, O; O: O? O!
            PAPBPCPDPEPFPGPHPIPJPK
            PLPMPNPOPPQPRPSPTPU
            PVPWPXPYPZP P’S P”
            P-P–PP—P…P. P, P; P: P? P!
            QAQBQCQDQEQFQGQHQIQJQK
            QLQMQNQOQPQQRQSQTQU
            QVQWQXQYQZQ Q’S Q”
            Q-Q–Q—Q…Q. Q, Q; Q: Q? Q!
            RARBRCRDRERFRGRHRIRJRK
            RLRMRNRORPRQRRSRTRU
            RVRWRXRYRZR R’S R”
            R-R–R—R…R. R, R; R: R? R!
            SASBSCSDSESFSGSHSISJSK
            SLSMSNSOSPSQSRSSTSU
            SVSWSXSYSZS S’S S”
            S-S–S—S…S. S, S; S: S? S!
            TATBTCTDTETFTGTHTITJTK
            TLTMTNTOTPTQTRTSTTU
            TVTWTXTYTZT T’S T”
            T-T–T—T…T. T, T; T: T? T!
            UAUBUCUDUEUFUGUHUIUJUK
            ULUMUNUOUPUQURUSUTUU
            UVUWUXUYUZU U’S U”
            U-U–U—U…U. U, U; U: U? U!
            VAVBVCVDVEVFVGVHVIVJVK
            VLVMVNVOVPVQVRVSVTVU
            VVWVXVYVZV V’S V”
            V-V–V—V…V. V, V; V: V? V!
            WAWBWCWDWEWFWGWHWIWJWK
            WLWMWNWOWPWQWRWSWTWU
            WVWWXWYWZW W’S W”
            W-W–W—W…W. W, W; W: W? W!
            XAXBXCXDXEXFXGXHXXIXJXK
            XLXMXNXOXPXQXRXSXTXU
            XVXWXXYXZX X’S X”
            X-X–X—X…X. X, X; X: X? X!
            YAYBYCYDYEYFYGYHYIYJYK
            YLYMYNYOYPYQYRYSYTYU
            YVYWYXYYZY Y’S Y”
            Y-Y–Y—Y…Y. Y, Y; Y: Y? Y!
            ZAZBZCZDZEZFZGZHZIZJZK
            ZLZMZNZOZPZQZRZSZTZU
            ZVZWZXZYZZ Z’S Z”
            Z-Z–Z—Z…Z. Z, Z; Z: Z? Z!


            LOWER CASE

            aabacadaeafagahaiajakalama
            anaoapaqarasatauavawaxayaza
            a…a. a, a; a: a! a? a-a–a—a’s a”
            babbcbdbebfbgbhbibjbkblbmb
            bnbobpbqbrbsbtbubvbwbxbybzb
            b…b. b, b; b: b! b? b-b–b—b’s b”
            cacbccdcecfcgchcicjckclcmc
            cncocpcqcrcsctcucvcwcxcyczc
            c…c. c, c; c: c! c? c-c–c—c’s c”
            dadbdcdddedfdgdhdidjdkdldmd
            dndodpdqdrdsdtdudvdwdxdydzd
            d…d. d, d; d: d! d? d-d–d—d’s d”
            eaebecedeeefegeheiejekeleme
            eneoepeqereseteuevewexeyeze
            e…e. e, e; e: e! e? e-e–e—e’s e”
            fafbfcfdfefffgfhfifjfkflfmf
            fnfofpfqfrfsftfufvfwfxfyfzf
            f…f. f, f; f: f! f? f-f–f—f’s f”
            gagbgcgdgegfggghgigjgkglgmg
            gngogpgqgrgsgtgugvgwgxgygzg
            g…g. g, g; g: g! g? g-g–g—g’s g”
            hahbhchdhehfhghhhihjhkhlhmh
            hnhohphqhrhshthuhvhwhxhyhzh
            h…h. h, h; h: h! h? h-h–h—h’s h”
            iaibicidieifigihiiijikiliminioipiqirisiti
            iuiviwixiyizi i…i. i, i; i: i! i? i-i–i—i’s i”
            jajbjcjdjejfjgjhjijjjkjljmjjnjojpjqjrjsjtjujvj
            jwjxjyjzj j…j. j, j; j: j! j? j-j–j—j’s j”
            kakbkckdkekkfkgkhkikjkkklkmknkok
            kpkqkrksktkukkvkwkxkykzk
            k…k. k, k; k: k! k? k-k–k—k’s k”
            lalblcldlelflglhliljlklllmlnlolplqlrlsltlulvl
            lwlxlylzl l…l. l, l; l: l! l? l-l–l—l’s l”
            mambmcmdmemfmgmhmimjmkm
            mlmmmnmompmqmrmsmtmumvm
            mwmxmymzm’s m”
            m…m. m, m; m: m! m? m-m–m—m
            nanbncndnenfngnhninjnknlnmn
            nnnonpnqnrnsntnunvnwnxnynzn
            n…n. n, n; n: n! n? n-n–n—n’s n”
            oaobocodoeofogohoiojokolomo
            onooopoqorosotouovowoxoyozo
            o…o. o, o; o: o! o? o-o–o—o’s o”
            papbpcpdpepfpgphpipjpkplpmp
            pnpopppqprpsptpupvpwpxpypzp
            p…p. p, p; p: p! p? p-p–p—p’s p”
            qaqbqcqdqeqfqgqhqiqjqkqlqmq
            qnqoqpqqqrqsqtquqvqwqxqyqzq
            q…q. q, q; q: q! q? q-q–q—q’s q”
            rarbrcrdrerfrgrhrirjrkrlrmr
            rnrorprqrrrsrtrurvrwrxryrzr
            r…r. r, r; r: r! r? r-r–r—r’s r”
            sasbscsdsesfsgshsisjskslsms
            snsospsqsrssstsusvswsxsyszs
            s…s. s, s; s: s! s? s-s–s—s’s s”
            tatbtctdtetftgthtitjtktltmt
            tntotptqtrtstttutvtwtxtytzt
            t…t. t, t; t: t! t? t-t–t—t’s t”
            uaubucudueufuguhuiujukulumu
            unuoupuqurusutuuuvuwuxuyuzu
            u…u. u, u; u: u! u? u-u–u—u’s u”
            vavbvcvdvevfvgvhvivjvkvlvmv
            vnvovpvqvrvsvtvuvvvwvxvyvzv
            v…v. v, v; v: v! v? v-v–v—v’s v”
            wawbwcwdwewfwgwhwiwjwkw
            wlwmwnwowpwqwrwswtwuwvw
            wwxwywzw’s w”
            w…w. w, w; w: w! w? w-w–w—w
            xaxbxcxdxexfxgxhxixjxkxlxmxnx
            xoxpxqxrxsxtxuxvxwxxxyxzx
            x…x. x, x; x: x! x? x-x–x—x’s x”
            yaybycydyeyfygyhyiyjykylymyny
            yoypyqyrysytyuyvywyxyyyzy
            y…y. y, y; y: y! y? y-y–y—y’s y”
            zazbzczdzezfzgzhzizjzkzlzmznz
            zozpzqzzrzsztzuzvzwzxzyzzz
            z…z. z, z; z: z! z? z-z–z—z’s z”


            UPPER & LOWER

            Aaron Able Ache Advert
            Aegis Aft Age Ahe Ails\s
            Ajar Akin Aloe Amish And
            Aone Ape Aqua Are Ascot
            Atlas Auto Avon Awe Axe\s
            Aye Azo Aí Aì Aî Aï
            Band Bet Bing Bloat Bog
            Bring Bumpy Byte Bí Bì Bî Bï
            Carry Celar Cinthia Cope Crap\s
            Cult Cycle Cí Cì Cî Cï
            Dark Demon Dingy Dope Dumb\s
            Dí Dì Dî Dï
            Each Eels Einar Eons Euchre\s
            Ever Ewer Exit Eyes Eí Eì Eî Eï
            Fact Fever Fire Fıne Font Framer\s
            Fur Fyrd Fähr Földer Fà Få Fál\s
            Fâl Fãl Fè Fé Fêl Fël Fíl Fìl Fîl Fïl\s
            Fól Fò Fôl Fõl Fúl Fù Fûl Fÿ Fünk\s
            Gayle Gentle Girl Gnome Gí Gì Gî Gï
            Gonot Grinning Gulf Gwen Gyro\s
            Harder Help Hilton Honor Hunk
            Hv Hymn Hí Hì Hî Hï
            Ian Ieo Iggy Iillian Ion Iugia Iyaaa
            Ií Iì Iî Iï\s
            Jacky Jester Jimmy Joint Junk \s
            Jí Jì Jî Jï
            Kangaroo Keep Kill Kline
            Kop Kudees Kwick Kva Kyle\s
            Kärn Köff Küdos Kì Kî Kï Kÿ
            Lay Learned Listing Load Lung
            Lw Lynch\s
            Mail Meal Minx Mode Music\s
            Myth Mí Mì Mî Mï
            Nail Next Nile Nooky Numb\s
            Ní Nì Nî Nï
            Oatmeal Oer Offer Ogor Oink Oolong\s
            Out Over Oyster Oí Oì Oî Oï
            Painter Peal Pile Pıne Pjb Plaster
            Pointer Printer Putt Pygmy Päl Pöl
            Pünk Pál Pà Pâl Pãl På Pél Pè\s
            Pêl Píl Pì Pîl Pïl Pól Pò Pôl
            Põl Púl Pù Pûl Pÿ
            Qanat Qels Qix Qon Quest\s
            Qí Qì Qî Qï
            Rate Red Right Royal Run\s
            Ryal Rän Röad Rüng
            Rí Rì Rî Rï
            Sallie Scuzz Sensation Shell Sink\s
            Smelly Snowball Soul Spoke Sqish\s
            Stoner Sung Svelt Swap Symantic
            Sí Sì Sî Sï
            Tail Teal Them Timer Tıme\s
            Toll Trustee Tsing Tumbs Twizzlers
            Typing Täp Törn Tüff Tál Tà Tâl\s
            Tãl Tål Tél Tè Têl Tël Tíl Tì Tîl\s
            Tïl Tól Tò Tôl Tõl Túl Tù Tûl Tÿ
            Uarco Ue Ui Umbrella Under Uo\s
            Upper Ursula User Utterly Uu Uv\s
            Uwe Uí Uì Uî Uï
            Vain Vc Veto Vine Vıne Vlad
            Vow Vroor Vs Vulgar Vying\s
            Vämping Vöter Vál Và Vâl Vãl Vå\s
            Vél Vè Vêl Vë Víl Vìl Vîl Vïl Vól\s
            Vò Vôl Vö Võl Vúl Vù Vûl Vü Vÿ
            Wale Wet What Window Wınd\s
            Wm Wow Wren Wsa Wte Wud\s
            Wxe Wynde Wäne Wál Wà Wâl
            Wãl Wå Wél Wè Wêl Wël Wíl\s
            Wìl Wîl Wïl Wól Wò Wôl Wöl\s
            Wÿ Wõl Wúl Wù Wûl Wül
            Xanth Xelo Xi Xo Xu Xylo\s
            Xál Xà Xâ Xä Xã Xå Xé Xè
            Xê Xë Xíl Xìl Xîl Xïl Xó Xò Xô\s
            Xö Xõ Xú Xù Xû Xü Xå Xÿ
            Yale Yddes Yearling Yicks Yıeld\s
            Yoyo Ypring Yrs Ys Yuck\s
            Yviye Yz Yá Yà Yâ Yä Yã Yå\s
            Yé Yè Yê Yë Yí Yì Yî Yï Yó\s
            Yò Yô Yö Yõ Yú Yù Yû Yü \s
            Zanzabar Zellis Zion Zope Zulu\s
            Zyle Zí Zì Zî Zï


            SIDE BEARINGS

            HIHOH
            HAHBHCHDHEH
            OAOBOCODOEO
            HFHGHHHIHJH
            OFOGOHOIOJO
            HKHLHMHNHOHPHON
            OKOLOMONOOPO
            HQUHRHSHTHUH
            OQOROSOTOUO
            HVHWHXHYHZH
            OVOWOXOYOZO
            HÌHÍHÏHÎHHÆHŒH
            HØHHÇHOÇO
            HIHOH  Hillolih
            nanbncndnenfngno
            oaobocodoeofogo
            nhninjnknlnmnnn
            ohoiojokolomono
            nonpnqnrnsntnun
            ooopoqorosotouo
            nvnwnxnynzn
            ovowoxoyozo
            nßnænœnønnçn
            oßoæoœoøooço
            líll lìll lîll lïll lildibıb
            √oﬁlloﬂilo≠o¶o∞H¬
            IOIH! IOIH? f! f? ¡H! ¿H?
            «n»«o»‹n›‹o›
            H&HO&O{H}H
            H•HO•OH@HªHº
            H⁄H‚H„H™H®H©H
            H·H O·O`´˘¨ˆ˜¯ˇ˙˚¸˝˛ˆ
            “HIOH”\s
            IOIH. H, H: H;
            O.O,O:O;‘O’
            ‘HIH’S “HIH” ‘H’\s
            (H)[H]{H}(O)[O]
            H/H\\H
            H£0H$0H0¢Hƒ7H¥7H
            H*H†H‡H§H
            IOH-HO-O
            H–HO–O
            H—HO—O
            1'1"0°H#7#0H
            00%00‰H…H
            H=H+H±H
            H0H1H2H3H4H5H
            H6H7H8H9HOH
            00110121314151
            16171819122
            202122324252
            2627282930


            CAPS & PUNCTUATION

            H…H. H: H, H; H! H?
            A…A. A: A, A; A! A?
            B…B. B: B, B; B! B?
            C…C. C: C, C; C! C?
            D…D. D: D, D; D! D?
            E…E. E: E, E; E! E?
            F…F. F: F, F; F! F?
            G…G. G: G, G; G! G?
            H…H. H: H, H; H! H?
            I…I. I: I, I; I! I?
            J…J. J: J, J; J! J?
            K…K. K: K, K; K! K?
            L…L. L: L, L; L! L?
            M…M. M: M, M; M! M?
            N…N. N: N, N; N! N?
            O…O. O: O, O; O! O?
            P…P. P: P, P; P! P?
            Q…Q. Q: Q, Q; Q! Q?
            R…R. R: R, R; R! R?
            S…S. S: S, S; S! S?
            T…T. T: T, T; T! T?
            U…U. U: U, U; U! U?
            V…V. V: V, V; V! V?
            W…W. W: W, W; W! W?
            X…X. X: X, X; X! X?
            Y…Y. Y: Y, Y; Y! Y?
            Z…Z. Z: Z, Z; Z! Z?

            H-H–H—H’S “H” ‘H’
            A-A–A—AA’S “A”
            B-B–B—BB’S “B”
            C-C–C—CC’S “C”
            D-D–D—DD’S “D”
            E-E–E—EE’S “E”
            F-F–F—FF’S “F”
            G-G–G—GG’S “G”
            H-H–H—HH’S “H”
            I-I–I—II’S “I”
            J-J–J—JJ’S “J”
            K-K–K—KK’S “K”
            L-L–L—LL’S “L”
            M-M–M—MM’S “M”
            N-N–N—NN’S “N”
            O-O–O—OO’S “O”
            P-P–P—PP’S “P”
            Q-Q–Q—QQ’S “Q”
            R-R–R—RR’S “R”
            S-S–S—SS’S “S”
            T-T–T—TT’S “T”
            U-U–U—UU’S “U”
            V-V–V—VV’S “V”
            W-W–W—WW’S “W”
            X-X–X—XX’S “X”
            Y-Y–Y—YY’S “Y”
            Z-Z–Z—ZZ’S “Z”


            LOWER CASE & PUNCTUATION

            H…H. H! H?
            l…l. l: l, l; l! l?
            a…a. a: a, a; a! a?
            b…b. b: b, b; b! b?
            c…c. c: c, c; c! c?
            d…d. d: d, d; d! d?
            e…e. e: e, e; e! e?
            f…f. f: f, f; f! f?
            g…g. g: g, g; g! g?
            h…h. h: h, h; h! h?
            i…i. i: i, i; i! i?
            j…j. j: j, j; j! j?
            k…k. k: k, k; k! k?
            l…l. l: l, l; l! l?
            m…m. m: m, m; m! m?
            n…n. n: n, n; n! n?
            o…o. o: o, o; o! o?
            p…p. p: p, p; p! p?
            q…q. q: q, q; q! q?
            r…r. r: r, r; r! r?
            s…s. s: s, s; s! s?
            t…t. t: t, t; t! t?
            u…u. u: u, u; u! u?
            v…v. v: v, v; v! v?
            w…w. w: w, w; w! w?
            x…x. x: x, x; x! x?
            y…y. y: y, y; y! y?
            z…z. z: z, z; z! z?

            H-H–H—H’S “H” ‘H’
            l-l–l—ll’s “l” ‘l’
            o-o–o—oso’s “o”
            a-a–a—asa’s “a”
            b-b–b—bsb’s “b”
            c-c–c—csc’s “c”
            d-d–d—dsd’s “d”
            e-e–e—ese’s “e”
            f-f–f—fsf’s “f”\s
            g-g–g—gsg’s “g”
            h-h–h—hsh’s “h”
            i-i–i—isi’s “i”
            j-j–j—jsj’s “j”
            k-k–k—ksk’s “k”
            l-l–l—lsl’s “l”
            m-m–m—msm’s “m”
            n-n–n—nsn’s “n”
            o-o–o—oso’s “o”
            p-p–p—psp’s “p”
            q-q–q—qsq’s “q”
            r-r–r—rsr’s “r”
            s-s–s—sss’s “s”
            t-t–t—tst’s “t”
            u-u–u—usu’s “u”
            v-v–v—vsv’s “v”
            w-w–w—wsw’s “w”
            x-x–x—xsx’s “x”
            y-y–y—ysy’s “y”
            z-z–z—zsz’s “z”


            INCIDENTALS

            A* B* C* D* E*\s
            F* G* H* I* J* K*
            L* M* N* O* P*\s
            Q* R* S* T* U*\s
            V* W* X* Y* Z*
            a* b* c* d* e*\s
            f* g* h* i* j* k*
            l* m* n* o* p*\s
            q* r* s* t* u*\s
            v* w* x* y* z*

            f.* f,* f;* f:*
            r.* r,* r;* r:*
            v.* v,* v;* v:*
            w.* w,* w;* w:*
            y.* y,* y;* y:*    \s
            T.* T,* T;* T:*
            V.* V,* V;* V:*
            W.* W,* W;* W:*
            Y.* Y,* Y;* Y:*
            f.” f,” f.’ f,’
            r.” r,” r.’ r,’\s
            v.” v,” v.’ v,’\s
            w.” w,” w.’ w,’\s
            y.” y,” y.’ y,’  \s
            T.” T,” T.’ T,’ \s
            V” V.” V,” V.’ V,’\s
            W.” W,” W.’ W,’\s
            Y.” Y,” Y.’ Y,’\s
            “…” ‘…’ “ ”\s
            “HIH!” !’\s
            “HIH?” ?’\s
            WE’D I’LL\s
            I’M TAM’O
            WE’RE WE'VE
            WHO’S WHAT'S
            AIN’T I’VE
            edWe’d I’d We’ll
            I’m tam’omo\s
            we’rer who’s\s
            isn’t I’ve
            ‘A’ ‘E’ ‘J’ ‘O’ ‘T’\s
            ‘V’ ‘W’ ‘Y’\s
            “A” “E” “J”\s
            “O” “T”
            “V” “W” “Y”\s

            /A/B/C/D/E/F/
            /G/H/I/J/K/L/\s
            /M/N/O/P/Q/
            /R/S/T/U/V/A\s
            /W/X/Y/Z/  \s
            /a/b/c/d/e/f/\s
            /g/h/i/j/k/l/m/
            /n/o/p/q/r/s/\s
            /t/u/v/w/x/y/z/\s
            /0/1/2/3/4/
            /5/6/7/8/9/
            \s
            A™ A® B™ B® C™ C®\s
            D™ D® E™ E® F™ F®
            G™ G® H™ H® I™ I®
            J™ J® K™ K® L™ L®\s
            M™ M® N™ N® O™ O®
            P™ P® Q™ Q® R™ R®
            S™ S® T™ T® U™ U®\s
            V™ V® W™ W® X™ X®
            Y™ Y® Z™ Z®\s
            a™ a® b™ b® c™ c®\s
            d™ d® e™ e® f™ f®
            g™ g® h™ h® i™ i®
            j™ j® k™ k® l™ l®\s
            m™ m® n™ n® o™ o®
            p™ p® q™ q® r™ r®
            s™ s® t™ t® u™ u®\s
            v™ v® w™ w® x™ x®\s
            y™ y® z™ z®

            001020304050
            0607080900
            101121314151
            16171819100
            202122324252
            26272829200
            303132334353
            36373839300
            404142434454
            46474849400
            505152535455
            56575859500
            606162636465
            6676869600
            707172737475
            7677879700
            808182838485
            8687889800
            909192939495
            9697989900
            (1)(2)(3)(4)(5)
            (6)(7)(8)(9)(0)
            \s
            $00 $10 $20 $30 $40\s
            $50 $60 $70 $80 $90\s
            £00 £10 £20 £30 £40\s
            £50 £60 £70 £80 £90\s
            ƒ00 ƒ10 ƒ20 ƒ30 ƒ40\s
            ƒ50 ƒ60 ƒ70 ƒ80 ƒ90
            00¢ 11¢ 22¢ 33¢ 44¢
            55¢ 66¢ 77¢ 88¢ 99¢
            \s
            00% 0‰ 0-0.0,0…0°\s
            11% 1‰ 1-1.1,1…1°
            00% 0‰ 0-0.0,0…0°\s
            12% 2‰ 2-2.2,2…2°
            13% 3‰ 3-3.3,3…3°
            11% 1‰ 1-1.1,1…1°
            14% 4‰ 4-4.4,4…4°
            15% 5‰ 5-5.5,5…5°
            16% 6‰ 6-6.6,6…6°
            11% 1‰ 1-1.1,1…1°
            17% 7‰ 7-7.7,7…7°
            18% 8‰ 8-8.8,8…8°
            19% 9‰ 9-9.9,9…9°

            """;
    public static final String FONT_FALLBACK_TEST = """
            Latin
            ABCDEFGHIJKLMNOPQRSTUVWXYZ
            abcdefghijklmnopqrstuvwxyz

            Latin 1 (Western)
            ÁÀÂÄÅÃÆÇÐÉÈÊËÍÌÎÏÑÓÒÔÖÕØŒÞÚÙÛÜÝŸ
            áàâäãåæçðéèêëíìîïıñóòôöõøœßþúùûüýÿ

            Latin 2 (Eastern)
            ĀĂĄĆČĎĐĒĖĘĚĞĢĪĮİĶŁĹĻĽŃŅŇŌŐŔŖŘŠŚŞȘŢȚŤŪŮŰŲŽŹŻ
            āăąćčďđēėęěğģīįķłĺļľńņňōőŕŗřšśşșţțťūůűųžźż

            Greek (Modern)
            ΑΒΓ∆ΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩΆΈΉΊΌΎΏΪΫ
            αβγδεζηθικλµνξοπρςστυφχψωάέήίόύώϊϋΐΰ

            Cyrillic 1 (Russian)
            АБВГДЕЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ
            абвгдежзийклмнопрстуфхцчшщъыьэюя

            Cyrillic 2 (Extended)
            ЀЁЂЃЄЅІЇЈЉЊЋЌЍЎЏҐӁƏҒҖҚҢҮҰҲҶҺӘӢӨӮ
            ѐёђѓєѕіїјљњћќѝўџґӂǝғҗқңүұҳҷһәӣөӯ
            Japanese
            Grade 1
            一 九 七 二 人 入 八 力 十 下 三 千 上 口 土 夕 大 女 子 小 山 川 五 天 中 六 円 手 文\s
            日 月 木 水 火 犬 王 正 出 本 右 四 左 玉 生 田 白 目 石 立 百 年 休 先 名 字 早 気 竹 糸\s
            耳 虫 村 男 町 花 見 貝 赤 足 車 学 林 空 金 雨 青 草 音 校 森
            Grade 2
            刀 万 丸 才 工 弓 内 午 少 元 今 公 分 切 友 太 引 心 戸 方 止 毛 父 牛 半 市 北 古 台 兄\s
            冬 外 広 母 用 矢 交 会 合 同 回 寺 地 多 光 当 毎 池 米 羽 考 肉 自 色 行 西 来 何 作 体 弟\s
            図 声 売 形 汽 社 角 言 谷 走 近 里 麦 画 東 京 夜 直 国 姉 妹 岩 店 明 歩 知 長 門 昼 前 南\s
            点 室 後 春 星 海 活 思 科 秋 茶 計 風 食 首 夏 弱 原 家 帰 時 紙 書 記 通 馬 高 強 教 理 細\s
            組 船 週 野 雪 魚 鳥 黄 黒 場 晴 答 絵 買 朝 道 番 間 雲 園 数 新 楽 話 遠 電 鳴 歌 算 語 読\s
            聞 線 親 頭 曜 顔
            Grade 3
            丁 予 化 区 反 央 平 申 世 由 氷 主 仕 他 代 写 号 去 打 皮 皿 礼 両 曲 向 州 全 次 安 守
             式 死 列 羊 有 血 住 助 医 君 坂 局 役 投 対 決 究 豆 身 返 表 事 育 使 命 味 幸 始 実 定
             岸 所 放 昔 板 泳 注 波 油 受 物 具 委 和 者 取 服 苦 重 乗 係 品 客 県 屋 炭 度 待 急 指
             持 拾 昭 相 柱 洋 畑 界 発 研 神 秒 級 美 負 送 追 面 島 勉 倍 真 員 宮 庫 庭 旅 根 酒 消
             流 病 息 荷 起 速 配 院 悪 商 動 宿 帳 族 深 球 祭 第 笛 終 習 転 進 都 部 問 章 寒 暑 植
             温 湖 港 湯 登 短 童 等 筆 着 期 勝 葉 落 軽 運 遊 開 階 陽 集 悲 飲 歯 業 感 想 暗 漢 福
             詩 路 農 鉄 意 様 緑 練 銀 駅 鼻 横 箱 談 調 橋 整 薬 館 題
            Chinese
            視申父泉禁名時資題敬足仲与京周可初明染下。克東験未音際保哉木大卓俳世。
            図反署必益明属前判庫男藤染米律児最特選描。事航版設丸井株需意登運名広。
            活率閣探紅容声安目青谷知激武講介。況取密満論訳朝長写端買総冬。逮会朝咲不朝温村験食公納生歩。
            京根狭書値裁給事認室健超幼読共法改。索洋物幅意写鳥輩全訴写考盟務少。""";
    public static final Icon FOLDER_ICON = AllIcons.Files.Folder.get(19, 19);
}
