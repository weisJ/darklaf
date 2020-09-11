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
        CSSBuilder builder = new CSSBuilder();

        // @formatter:off
        builder.group("body")
                    .fontSize(pt(14))
                    .fontFamily(font.getFamily(), font.getName())
                    .fontWeight("normal")
                    .marginRight(0)
                    .marginLeft(0)
                    .color(hex("textForeground"))
               .group("p")
                    .marginTop(px(15))
               .group("h1")
                   .fontSize("x-large")
                   .fontWeight("bold")
                   .marginTop(px(10))
                   .marginBottom(px(10))
               .group("h2")
                   .fontSize("large")
                   .fontWeight("bold")
                   .marginTop(px(10))
                   .marginBottom(px(10))
               .group("h3")
                   .fontSize("medium")
                   .fontWeight("bold")
                   .marginTop(px(10))
                   .marginBottom(px(10))
               .group("h4")
                   .fontSize("small")
                   .fontWeight("bold")
                   .marginTop(px(10))
                   .marginBottom(px(10))
               .group("h5")
                   .fontSize("x-small")
                   .fontWeight("bold")
                   .marginTop(px(10))
                   .marginBottom(px(10))
               .group("h6")
                   .fontSize("xx-small")
                   .fontWeight("bold")
                   .marginTop(px(10))
                   .marginBottom(px(10))
               .group("li p")
                   .marginTop(0)
                   .marginBottom(0)
                   .group("td p")
                   .marginTop(0)
               .group("menu li p")
                   .marginTop(0)
                   .marginBottom(0)
               .group("menu li")
                   .margin(0)
               .group("menu")
                   .marginLeftLTR(px(40))
                   .marginRightRTL(px(40))
                   .marginTop(px(10))
                   .marginBottom(px(10))
               .group("dir li p")
                   .marginTop(0)
                   .marginBottom(0)
               .group("dir li")
                   .margin(0)
               .group("dir")
                   .marginLeftLTR(px(40))
                   .marginRightRTL(px(40))
                   .marginTop(px(10))
                   .marginBottom(px(10))
               .group("dd")
                   .marginLeftLTR(px(40))
                   .marginRightRTL(px(40))
                   .marginTop(px(10))
                   .marginBottom(px(10))
               .group("dd p")
                   .margin(0)
               .group("dt")
                   .marginTop(0)
                   .marginBottom(0)
               .group("dl")
                   .marginLeft(0)
                   .marginTop(px(10))
                   .marginBottom(px(10))
               .group("ol li")
                   .margin(0)
               .group("ol")
                   .marginTop(px(10))
                   .marginBottom(px(10))
                   .marginLeftLTR(px(50))
                   .marginRightRTL(px(50))
                   .listStyleType("decimal")
               .group("ol li p")
                   .marginTop(0)
                   .marginBottom(0)
               .group("ul li")
                   .margin(0)
               .group("ul")
                   .marginTop(px(10))
                   .marginBottom(px(10))
                   .marginLeftLTR(px(50))
                   .marginRightRTL(px(50))
                   .listStyleType("disc")
                   .property("-bullet-gap", px(10))
               .group("ul li ul li")
                   .margin(0)
               .group("ul li ul")
                   .marginLeftLTR(px(25))
                   .marginRightRTL(px(25))
                   .listStyleType("circle")
               .group("ul li ul li ul li")
                   .margin(0)
               .group("ul li menu")
                   .marginLeftLTR(px(25))
                   .marginRightRTL(px(25))
                   .listStyleType("circle")
               .group("ul li p")
                   .marginTop(0)
                   .marginBottom(0)
               .group("a")
                   .color(hex("hyperlink"))
                   .textDecoration("underline")
               .group("address")
                   .color(hex("hyperlink"))
                   .fontStyle("italic")
               .group("big")
                   .fontSize("x-large")
                   .group("small")
                   .fontSize("x-small")
               .group("samp")
                   .fontSize("small")
                   .fontFamily("Monospaced", "monospace")
               .group("code")
                   .fontSize("small")
                   .fontFamily("Monospaced", "monospace")
               .group("kbd")
                   .fontSize("small")
                   .fontFamily("Monospaced", "monospace")
               .group("cite")
                   .fontStyle("italic")
               .group("dfn")
                   .fontStyle("italic")
               .group("em")
                   .fontStyle("italic")
               .group("i")
                   .fontStyle("italic")
               .group("b")
                   .fontWeight("bold")
               .group("strong")
                   .fontWeight("bold")
               .group("strike")
                   .textDecoration("line-through")
               .group("s")
                   .textDecoration("line-through")
               .group("sub")
                   .property("vertical-align", "sub")
               .group("sup")
                   .property("vertical-align", "sub")
               .group("tt")
                   .fontFamily("Monospaced", "monospace")
               .group("u")
                   .textDecoration("underline")
               .group("var")
                   .fontWeight("bold")
                   .fontStyle("italic")
               .group("table")
                   .borderColor(hex("border"))
                   .borderStyle("solid")
                   .borderWidth(px(1))
                   .borderCollapse("collapse")
               .group("td")
                   .borderColor(hex("border"))
                   .borderStyle("none")
                   .borderWidth(px(1))
                   .padding(px(3))
                   .borderCollapse("collapse")
               .group("th")
                   .borderColor(hex("border"))
                   .borderStyle("solid")
                   .borderWidth(px(1))
                   .padding(px(3))
                   .fontWeight("bold")
                   .borderCollapse("collapse")
               .group("tr")
                   .property("text-align", "left")
               .group("blockquote")
                   .margin(px(5) + " " + px(35))
               .group("center")
                   .property("text-align", "center")
               .group("pre")
                   .marginTop(px(5))
                   .marginBottom(px(5))
                   .fontFamily("Monospaced", "monospace")
               .group("pre p")
                   .marginTop(0)
               .group("caption")
                   .property("caption-side", "top")
                   .property("text-align", "center")
               .group("nobr")
                   .property("white-space", "nowrap")
               .group("input")
                   .border("none")
               .group("div")
                   .borderColor(hex("borderSecondary"))
                   .borderWidth(px(1));
        // @formatter:on

        styleSheet.addRule(builder.toString());
        StyleSheet custom = currentTheme.loadStyleSheet();
        if (custom.getStyleNames().hasMoreElements()) {
            styleSheet.addStyleSheet(custom);
        }
        new HTMLEditorKit().setStyleSheet(styleSheet);
    }

    protected String hex(final String key) {
        Color c = defaults.getColor(key);
        return "#" + ColorUtil.toHex(c);
    }

    protected String px(final int px) {
        return px + "px";
    }

    protected String pt(final int pt) {
        return pt + "pt";
    }

    @Override
    public boolean onlyDuringInstallation() {
        return true;
    }
}
