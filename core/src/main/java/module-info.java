/*
 * MIT License
 *
 * Copyright (c) 2021 Jannis Weis
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
module darklaf.core {
    requires transitive java.desktop;
    requires transitive darklaf.theme;
    requires transitive darklaf.properties;
    requires transitive darklaf.utils;

    requires darklaf.nativeUtils;
    requires darklaf.platform.base;
    requires darklaf.platform.windows;
    requires darklaf.platform.macos;
    requires jxlayer;
    requires swing.extensions.laf.support;
    requires jdk.unsupported.desktop;

    requires static annotations;
    requires static swingx;

    exports com.github.weisj.darklaf;
    exports com.github.weisj.darklaf.components;
    exports com.github.weisj.darklaf.components.alignment;
    exports com.github.weisj.darklaf.components.border;
    exports com.github.weisj.darklaf.components.button;
    exports com.github.weisj.darklaf.components.chooser;
    exports com.github.weisj.darklaf.components.color;
    exports com.github.weisj.darklaf.components.help;
    exports com.github.weisj.darklaf.components.iconeditor;
    exports com.github.weisj.darklaf.components.loading;
    exports com.github.weisj.darklaf.components.popup;
    exports com.github.weisj.darklaf.components.renderer;
    exports com.github.weisj.darklaf.components.tabframe;
    exports com.github.weisj.darklaf.components.text;
    exports com.github.weisj.darklaf.components.togglebuttonlist;
    exports com.github.weisj.darklaf.components.tooltip;
    exports com.github.weisj.darklaf.components.tree;

    exports com.github.weisj.darklaf.focus;
    exports com.github.weisj.darklaf.graphics;
    exports com.github.weisj.darklaf.listener;
    exports com.github.weisj.darklaf.settings;
    exports com.github.weisj.darklaf.task;

    exports com.github.weisj.darklaf.ui;
    exports com.github.weisj.darklaf.ui.button;
    exports com.github.weisj.darklaf.ui.colorchooser;
    exports com.github.weisj.darklaf.ui.combobox;
    exports com.github.weisj.darklaf.ui.list;
    exports com.github.weisj.darklaf.ui.progressbar;
    exports com.github.weisj.darklaf.ui.rootpane;
    exports com.github.weisj.darklaf.ui.scrollpane;
    exports com.github.weisj.darklaf.ui.slider;
    exports com.github.weisj.darklaf.ui.spinner;
    exports com.github.weisj.darklaf.ui.splitpane;
    exports com.github.weisj.darklaf.ui.table;
    exports com.github.weisj.darklaf.ui.text;
    exports com.github.weisj.darklaf.ui.togglebutton;
    exports com.github.weisj.darklaf.ui.toolbar;
    exports com.github.weisj.darklaf.ui.tooltip;
    exports com.github.weisj.darklaf.ui.tree;
}
