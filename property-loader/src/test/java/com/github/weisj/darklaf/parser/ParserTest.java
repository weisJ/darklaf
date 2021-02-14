/*
 * MIT License
 *
 * Copyright (c) 2020 Jannis Weis
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
 *
 */
package com.github.weisj.darklaf.parser;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Insets;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;

import javax.swing.UIDefaults;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.github.weisj.darklaf.color.ColorUtil;
import com.github.weisj.darklaf.icons.IconLoader;

class ParserTest {

    final ParserContext context = new ParserContext(
            new UIDefaults(), new UIDefaults(),
            IconLoader.get(ParserTest.class));

    @BeforeEach
    void setup() {
        context.defaults.clear();
        context.accumulator.clear();
    }

    Object parse(final String key, final String value) {
        return parse(key, value, true);
    }

    Object parse(final String key, final String value, final boolean shouldSucceed) {
        ParseResult p = Parser.parse(new ParseResult(key, value), context);
        Assertions.assertEquals(shouldSucceed, p.finished);
        return p.result;
    }

    @Test
    void testPrimitives() {
        Assertions.assertEquals("Hello World", parse("key", "Hello World"));

        Assertions.assertEquals(false, parse("key", "false"));
        Assertions.assertEquals(false, parse("key", "FALSE"));
        Assertions.assertEquals(false, parse("key", "FaLsE"));

        Assertions.assertEquals(true, parse("key", "true"));
        Assertions.assertEquals(true, parse("key", "TRUE"));
        Assertions.assertEquals(true, parse("key", "tRuE"));

        for (int i = -100; i < 100; i++) {
            Assertions.assertEquals(i, parse("key", String.valueOf(i)));
        }

        Random r = new Random();
        for (int i = 0; i < 100; i++) {
            Color c = new Color(r.nextInt(256), r.nextInt(256), r.nextInt(256));
            Assertions.assertEquals(c, parse("key", PrimitiveParser.COLOR_PREFIX + ColorUtil.toHex(c)));
        }
    }

    @Test
    void testColorLegacy() {
        Random r = new Random();
        for (int i = 0; i < 100; i++) {
            Color c = new Color(r.nextInt(256), r.nextInt(256), r.nextInt(256));
            Assertions.assertEquals(c, parse("key", ColorUtil.toHex(c)));
        }
    }

    @Test
    void testReferences() {
        Assertions.assertEquals("key", Parser.parse(new ParseResult("%key", "value"), context).key);

        Object obj = new Object();

        context.accumulator.put("key", obj);
        Assertions.assertEquals(obj, parse("key2", "%key"));

        context.accumulator.clear();
        context.defaults.put("key", obj);
        Assertions.assertEquals(obj, parse("key2", "%key"));
    }

    @Test
    void testFallbacks() {
        Object obj = new Object();
        context.accumulator.put("key1", obj);
        Assertions.assertEquals(obj, parse("key1", "?:false"));
        Assertions.assertEquals(false, parse("key2", "?:false"));

        Object obj2 = new Object();
        context.accumulator.put("key2", obj2);
        Assertions.assertEquals(obj, parse("key3", "?:%key1"));
    }

    @Test
    void testDimension() {
        Random r = new Random();
        for (int i = 0; i < 100; i++) {
            int w = r.nextInt();
            int h = r.nextInt();
            // While legacy declarations are supported these may be a valid color value.
            // and parse incorrectly.
            if (isValidColor(w)) continue;
            if (isValidColor(h)) continue;
            Assertions.assertEquals(new Dimension(w, h), parse("test.size", w + "," + h));
            Assertions.assertEquals(new Dimension(w, h), parse("testSize", w + "," + h));
        }

        Dimension dim = new Dimension(12, 34);
        context.accumulator.put("key.width", dim.width);
        Assertions.assertEquals(dim, parse("key.size", "%key.width," + dim.height));
    }

    private boolean isValidColor(final int value) {
        return ColorUtil.fromHex(String.valueOf(value), null) != null;
    }

    @Test
    void testInsets() {
        Random r = new Random();
        for (int i = 0; i < 100; i++) {
            Insets insets = new Insets(r.nextInt(), r.nextInt(), r.nextInt(), r.nextInt());

            // While legacy declarations are supported these may be a valid color value.
            // and parse incorrectly.
            if (isValidColor(insets.left)) continue;
            if (isValidColor(insets.right)) continue;
            if (isValidColor(insets.bottom)) continue;
            if (isValidColor(insets.right)) continue;
            Assertions.assertEquals(insets, parse("test.insets",
                    insets.top + "," + insets.left + "," + insets.bottom + "," + insets.right));
            Assertions.assertEquals(insets, parse("testInsets",
                    insets.top + "," + insets.left + "," + insets.bottom + "," + insets.right));
            Assertions.assertEquals(insets, parse("test.margins",
                    insets.top + "," + insets.left + "," + insets.bottom + "," + insets.right));
        }

        Insets ins = new Insets(1, 2, 3, 4);
        context.accumulator.put("key.left", ins.left);
        context.accumulator.put("key.bottom", ins.bottom);
        Assertions.assertEquals(ins, parse("key.insets", ins.top + ",%key.left,%key.bottom," + ins.right));
    }

    @Test
    void testActiveObject() {
        context.accumulator.put("key", parse("key.component", "java.lang.Object"));
        Assertions.assertEquals(Object.class, context.accumulator.get("key").getClass());
        Assertions.assertNotEquals(context.accumulator.get("key"), context.accumulator.get("key"));

        context.accumulator.put("key2", parse("key2.component", "%key"));
        Assertions.assertEquals(Object.class, context.accumulator.get("key2").getClass());
        Assertions.assertNotEquals(context.accumulator.get("key"), context.accumulator.get("key2"));
        if (context.accumulator instanceof UIDefaults) {
            // UIDefaults evaluates the active object when resolving the reference.
            // Hence active values get lost.
            Assertions.assertEquals(context.accumulator.get("key2"), context.accumulator.get("key2"));
        } else {
            Assertions.assertNotEquals(context.accumulator.get("key2"), context.accumulator.get("key2"));
        }
    }

    @Test
    void testLazyObject() {
        context.accumulator.put("key", parse("key.border", "java.lang.Object"));
        Assertions.assertEquals(Object.class, context.accumulator.get("key").getClass());
        Assertions.assertEquals(context.accumulator.get("key"), context.accumulator.get("key"));

        context.accumulator.put("key2", parse("key2.component", "%key"));
        Assertions.assertEquals(Object.class, context.accumulator.get("key2").getClass());
        Assertions.assertEquals(context.accumulator.get("key"), context.accumulator.get("key2"));
        Assertions.assertEquals(context.accumulator.get("key2"), context.accumulator.get("key2"));
    }

    @Test
    void testList() {
        Object obj = new Object();
        context.accumulator.put("key", obj);
        List<Object> list = Arrays.asList("Test", false, 15, obj);
        Assertions.assertEquals(list, parse("listKey", "[Test,false,15,%key]"));

        List<Object> nestedList =
                Arrays.asList(1, 2, Arrays.asList(3, Arrays.asList(4, 5), 6, 7), 8, Collections.singletonList(9), 10);
        Assertions.assertEquals(nestedList, parse("listKey", "[1,2,[3,[4,5],6,7],8,[9],10]"));
    }

    @Test
    void testMap() {
        Map<Object, Object> map = new HashMap<>();
        map.put("key1", 1);
        map.put("key2", false);
        map.put(3, "value3");
        map.put("key4", Arrays.asList(1, 2, 3));
        Assertions.assertEquals(map, parse("mapKey", "{key1:1,key2:false,3:value3,key4:[1,2,3]}"));
    }

    @Test
    void testDelimitedSplit() {
        String value1 = "a,b,[c,d,{e,f},g],(h),i,j)Suffix";
        String value2 = "Prefix(a,b,[c,d,{e,f},g],(h),i,j";
        List<String> expected = Arrays.asList("a", "b", "[c,d,{e,f},g]", "(h)", "i", "j");

        List<String> values1 = ParserUtil.delimitedSplit(',', ')', new ParseResult("key", value1), true);
        Assertions.assertEquals(expected, values1);

        Collections.reverse(expected);
        List<String> values2 = ParserUtil.delimitedSplit(',', '(', new ParseResult("key", value2), false);
        Assertions.assertEquals(expected, values2);
    }
}
