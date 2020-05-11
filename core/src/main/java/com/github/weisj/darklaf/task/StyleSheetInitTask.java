/*
 * MIT License
 *
 * Copyright (c) 2020 Jannis Weis
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 */
package com.github.weisj.darklaf.task;

import java.awt.*;

import javax.swing.*;
import javax.swing.text.html.HTMLEditorKit;
import javax.swing.text.html.StyleSheet;

import com.github.weisj.darklaf.theme.Theme;
import com.github.weisj.darklaf.util.ColorUtil;

public class StyleSheetInitTask implements DefaultsInitTask {

    private UIDefaults defaults;

    @Override
    public void run(final Theme currentTheme, final UIDefaults defaults) {
        this.defaults = defaults;
        StyleSheet styleSheet = new StyleSheet();
        Font font = defaults.getFont("html.font");
        StringBuilder b = new StringBuilder();
        b.append("body {")
         .append("font-size: 14pt;")
         .append("font-family: ").append(font.getFamily()).append(", ").append(font.getFontName()).append(";")
         .append("font-weight: normal;")
         .append("margin-left: 0;")
         .append("margin-right: 0;")
         .append("color: ").append(hex("textForeground")).append(";")
         .append("}");
        makeSingle(b, "p", "margin-top", "15px");
        makeHeading(b, 1, "x-large");
        makeHeading(b, 2, "large");
        makeHeading(b, 3, "medium");
        makeHeading(b, 4, "small");
        makeHeading(b, 5, "x-small");
        makeHeading(b, 6, "xx-small");
        makeTBMargins(b, "li p");
        makeSingle(b, "td p", "margin-top", "0");
        makeTBMargins(b, "menu li p");
        makeZeroMargins(b, "menu li");
        makeMargins(b, "menu");
        makeTBMargins(b, "dir li p");
        makeZeroMargins(b, "dir li");
        makeMargins(b, "dir");
        makeMargins(b, "dd");
        makeZeroMargins(b, "dd p");
        makeTBMargins(b, "dt");
        b.append("dl {")
         .append("margin-left: 0;")
         .append("margin-top: 10px;")
         .append("margin-bottom: 10px;")
         .append("}");
        makeZeroMargins(b, "ol li");
        b.append("ol {")
         .append("margin-top: 10px;")
         .append("margin-bottom: 10px;")
         .append("margin-left-ltr: 50px;")
         .append("margin-right-rtl: 50px;")
         .append("list-style-type: decimal;")
         .append("}");
        makeTBMargins(b, "ol li p");
        makeZeroMargins(b, "ul li");
        b.append("ul {")
         .append("margin-top: 10px;")
         .append("margin-bottom: 10px;")
         .append("margin-left-ltr: 50px;")
         .append("margin-right-rtl: 50px;")
         .append("list-style-type: disc;")
         .append("-bullet-gap: 10px;")
         .append("}");
        makeZeroMargins(b, "ul li ul li");
        b.append("ul li ul {")
         .append("margin-left-ltr: 25px;")
         .append("margin-right-rtl: 25px;")
         .append("list-style-type: circle;")
         .append("}");
        makeZeroMargins(b, "ul li ul li ul li");
        b.append("ul li menu {")
         .append("margin-left-ltr: 25px;")
         .append("margin-right-rtl: 25px;")
         .append("list-style-type: circle;")
         .append("}");
        makeTBMargins(b, "ul li p");
        b.append("a {")
         .append("color: ").append(hex("hyperlink")).append(";")
         .append("text-decoration: underline;")
         .append("}");
        b.append("address {")
         .append("color: ").append(hex("hyperlink"))
         .append("font-style: italic;")
         .append("}");
        makeSingle(b, "big", "font-size", "x-large");
        makeSingle(b, "small", "font-size", "x-small");
        makeMono(b, "samp");
        makeItalic(b, "cite");
        makeMono(b, "code");
        makeItalic(b, "dfn");
        makeItalic(b, "em");
        makeItalic(b, "i");
        makeBold(b, "b");
        makeMono(b, "kbd");
        makeSingle(b, "strike", "text-decoration", "line-through");
        makeSingle(b, "s", "text-decoration", "line-through");
        makeBold(b, "strong");
        makeSingle(b, "sub", "vertical-align", "sub");
        makeSingle(b, "sup", "vertical-align", "sub");
        makeSingle(b, "tt", "font-family", "Monospaced, monospace");
        makeSingle(b, "u", "text-decoration", "underline");
        b.append("var {")
         .append("font-weight: bold;")
         .append("font-style: italic;")
         .append("}");
        b.append("table {")
         .append("border-color: ").append(hex("border")).append(";")
         .append("border-style: solid;")
         .append("border-width: 1px;")
         .append("border-collapse: collapse;")
         .append("}");
        makeSingle(b, "tr", "text-align", "left");
        b.append("td {")
         .append("border-color: ").append(hex("border")).append(";")
         .append("border-style: none;")
         .append("border-width: 1px;")
         .append("padding: 3px;")
         .append("border-collapse: collapse;")
         .append("}");
        b.append("th {")
         .append("border-color: ").append(hex("border")).append(";")
         .append("border-style: solid;")
         .append("border-width: 1px;")
         .append("font-weight: bold;")
         .append("border-collapse: collapse;")
         .append("padding: 3px;")
         .append("}");
        makeSingle(b, "blockquote", "margin", "5px 35px");
        makeSingle(b, "center", "text-align", "center");
        b.append("pre {")
         .append("margin-top: 5px;")
         .append("margin-bottom: 5px;")
         .append("font-family: Monospaced, monospace;")
         .append("}");
        makeSingle(b, "pre p", "margin-top", "0");
        b.append("caption {")
         .append("caption-side: top;")
         .append("text-align: center;")
         .append("}");
        makeSingle(b, "nobr", "white-space", "nowrap");
        makeSingle(b, "input", "border", "none");
        b.append("div {")
         .append("border-color: ").append(hex("borderSecondary")).append(";")
         .append("border-width: 1px;")
         .append("}");
        styleSheet.addRule(b.toString());
        StyleSheet custom = currentTheme.loadStyleSheet();
        if (custom.getStyleNames().hasMoreElements()) {
            styleSheet.addStyleSheet(custom);
        }
        new HTMLEditorKit().setStyleSheet(styleSheet);
    }

    protected void makeHeading(final StringBuilder b, final int type, final String size) {
        b.append("h").append(type).append("{")
         .append("font-size: ").append(size).append(";")
         .append("font-weight: bold;")
         .append("margin-top: 10px;")
         .append("margin-bottom: 10px;")
         .append("}");
    }

    protected void makeMargins(final StringBuilder b, final String name) {
        b.append(name).append(" {")
         .append("margin-left-ltr: 40px;")
         .append("margin-right-rtl: 40px;")
         .append("margin-top: 10px;")
         .append("margin-bottom: 10px;")
         .append("}");
    }

    protected void makeTBMargins(final StringBuilder b, final String name) {
        b.append(name).append(" {")
         .append("margin-top: 0;")
         .append("margin-bottom: 0;")
         .append("}");
    }

    protected void makeZeroMargins(final StringBuilder b, final String name) {
        makeSingle(b, name, "margin", "0");
    }

    protected void makeItalic(final StringBuilder b, final String name) {
        makeSingle(b, name, "font-style", "italic");
    }

    protected void makeBold(final StringBuilder b, final String name) {
        makeSingle(b, name, "font-weight", "bold");
    }

    protected void makeSingle(final StringBuilder b, final String name,
                              final String key, final String value) {
        b.append(name).append(" {")
         .append(key).append(": ").append(value).append(";")
         .append("}");
    }

    protected void makeMono(final StringBuilder b, final String name) {
        b.append(name).append(" {")
         .append("font-size: small;")
         .append("font-family: Monospaced, monospace;")
         .append("}");
    }

    protected String hex(final String key) {
        Color c = defaults.getColor(key);
        return "#" + ColorUtil.toHex(c);
    }

    @Override
    public boolean onlyDuringInstallation() {
        return true;
    }
}
