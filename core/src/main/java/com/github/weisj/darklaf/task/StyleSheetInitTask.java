/*
 * MIT License
 *
 * Copyright (c) 2021 Jannis Weis
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

import com.github.weisj.darklaf.color.ColorUtil;
import com.github.weisj.darklaf.theme.Theme;

public class StyleSheetInitTask implements DefaultsInitTask {

    private UIDefaults defaults;

    @Override
    public void run(final Theme currentTheme, final UIDefaults defaults) {
        this.defaults = defaults;
        StyleSheet styleSheet = new StyleSheet();
        Font font = defaults.getFont("html.font");
        CSSBuilder builder = new CSSBuilder();

        builder.group("body")
                .fontSize(pt(14))
                .fontFamily(font.getFamily(), font.getName())
                .fontWeight("normal")
                .marginRight(0)
                .marginLeft(0)
                .color(hex("textForeground"));
        builder.group("p")
                .marginTop(px(15));
        builder.group("h1")
                .fontSize("x-large")
                .fontWeight("bold")
                .marginTop(px(10))
                .marginBottom(px(10));
        builder.group("h2")
                .fontSize("large")
                .fontWeight("bold")
                .marginTop(px(10))
                .marginBottom(px(10));
        builder.group("h3")
                .fontSize("medium")
                .fontWeight("bold")
                .marginTop(px(10))
                .marginBottom(px(10));
        builder.group("h4")
                .fontSize("small")
                .fontWeight("bold")
                .marginTop(px(10))
                .marginBottom(px(10));
        builder.group("h5")
                .fontSize("x-small")
                .fontWeight("bold")
                .marginTop(px(10))
                .marginBottom(px(10));
        builder.group("h6")
                .fontSize("xx-small")
                .fontWeight("bold")
                .marginTop(px(10))
                .marginBottom(px(10));
        builder.group("li p")
                .marginTop(0)
                .marginBottom(0);
        builder.group("td p")
                .marginTop(0);
        builder.group("menu li p")
                .marginTop(0)
                .marginBottom(0);
        builder.group("menu li")
                .margin(0);
        builder.group("menu")
                .marginLeftLTR(px(40))
                .marginRightRTL(px(40))
                .marginTop(px(10))
                .marginBottom(px(10));
        builder.group("dir li p")
                .marginTop(0)
                .marginBottom(0);
        builder.group("dir li")
                .margin(0);
        builder.group("dir")
                .marginLeftLTR(px(40))
                .marginRightRTL(px(40))
                .marginTop(px(10))
                .marginBottom(px(10));
        builder.group("dd")
                .marginLeftLTR(px(40))
                .marginRightRTL(px(40))
                .marginTop(px(10))
                .marginBottom(px(10));
        builder.group("dd p")
                .margin(0);
        builder.group("dt")
                .marginTop(0)
                .marginBottom(0);
        builder.group("dl")
                .marginLeft(0)
                .marginTop(px(10))
                .marginBottom(px(10));
        builder.group("ol li")
                .margin(0);
        builder.group("ol")
                .marginTop(px(10))
                .marginBottom(px(10))
                .marginLeftLTR(px(50))
                .marginRightRTL(px(50))
                .listStyleType("decimal");
        builder.group("ol li p")
                .marginTop(0)
                .marginBottom(0);
        builder.group("ul li")
                .margin(0);
        builder.group("ul")
                .marginTop(px(10))
                .marginBottom(px(10))
                .marginLeftLTR(px(50))
                .marginRightRTL(px(50))
                .listStyleType("disc")
                .property("-bullet-gap", px(10));
        builder.group("ul li ul li")
                .margin(0);
        builder.group("ul li ul")
                .marginLeftLTR(px(25))
                .marginRightRTL(px(25))
                .listStyleType("circle");
        builder.group("ul li ul li ul li")
                .margin(0);
        builder.group("ul li menu")
                .marginLeftLTR(px(25))
                .marginRightRTL(px(25))
                .listStyleType("circle");
        builder.group("ul li p")
                .marginTop(0)
                .marginBottom(0);
        builder.group("a")
                .color(hex("hyperlink"))
                .textDecoration("underline");
        builder.group("address")
                .color(hex("hyperlink"))
                .fontStyle("italic");
        builder.group("big")
                .fontSize("x-large");
        builder.group("small")
                .fontSize("x-small");
        builder.group("samp")
                .fontSize("small")
                .fontFamily("Monospaced", "monospace");
        builder.group("code")
                .fontSize("small")
                .fontFamily("Monospaced", "monospace");
        builder.group("kbd")
                .fontSize("small")
                .fontFamily("Monospaced", "monospace");
        builder.group("cite")
                .fontStyle("italic");
        builder.group("dfn")
                .fontStyle("italic");
        builder.group("em")
                .fontStyle("italic");
        builder.group("i")
                .fontStyle("italic");
        builder.group("b")
                .fontWeight("bold");
        builder.group("strong")
                .fontWeight("bold");
        builder.group("strike")
                .textDecoration("line-through");
        builder.group("s")
                .textDecoration("line-through");
        builder.group("sub")
                .property("vertical-align", "sub");
        builder.group("sup")
                .property("vertical-align", "sub");
        builder.group("tt")
                .fontFamily("Monospaced", "monospace");
        builder.group("u")
                .textDecoration("underline");
        builder.group("var")
                .fontWeight("bold")
                .fontStyle("italic");
        builder.group("table")
                .borderStyle("none")
                .borderCollapse("collapse");
        builder.group("td")
                .borderColor(hex("border"))
                .borderStyle("none")
                .borderWidth(px(1))
                .padding(px(3))
                .borderCollapse("collapse");
        builder.group("th")
                .borderColor(hex("border"))
                .borderStyle("solid")
                .borderWidth(px(1))
                .padding(px(3))
                .fontWeight("bold")
                .borderCollapse("collapse");
        builder.group("tr")
                .property("text-align", "left");
        builder.group("blockquote")
                .margin(px(5) + " " + px(35));
        builder.group("center")
                .property("text-align", "center");
        builder.group("pre")
                .marginTop(px(5))
                .marginBottom(px(5))
                .fontFamily("Monospaced", "monospace");
        builder.group("pre p")
                .marginTop(0);
        builder.group("caption")
                .property("caption-side", "top")
                .property("text-align", "center");
        builder.group("nobr")
                .property("white-space", "nowrap");
        builder.group("input")
                .border("none");
        builder.group("div")
                .borderColor(hex("borderSecondary"))
                .borderWidth(px(1));

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
