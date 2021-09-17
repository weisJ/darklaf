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

    requires darklaf.compatibility;
    requires darklaf.iconset;
    requires darklaf.nativeutil;
    requires darklaf.platform.base;
    requires darklaf.platform.windows;
    requires darklaf.platform.macos;
    requires swingdsl.laf;
    requires swingdsl.visualPadding;

    requires static annotations;
    requires static com.google.auto.service;
    requires static swingx;

    uses com.github.weisj.darklaf.theme.Theme;

    // Open resource bundles for java.desktop
    opens com.github.weisj.darklaf.task;

    // Open properties to PropertyLoader
    opens com.github.weisj.darklaf to darklaf.properties;
    opens com.github.weisj.darklaf.platform to darklaf.properties;
    opens com.github.weisj.darklaf.ui to darklaf.properties;

    provides com.github.weisj.darklaf.theme.laf.SynthesisedThemedLaf.ThemedLafProvider with
        com.github.weisj.darklaf.synthesised.ThemedDarkLafProvider;

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
    exports com.github.weisj.darklaf.components.tristate;

    exports com.github.weisj.darklaf.focus;
    exports com.github.weisj.darklaf.graphics;
    exports com.github.weisj.darklaf.listener;
    exports com.github.weisj.darklaf.settings;
    exports com.github.weisj.darklaf.task;
    exports com.github.weisj.darklaf.layout;

    exports com.github.weisj.darklaf.ui;
    exports com.github.weisj.darklaf.ui.button;
    exports com.github.weisj.darklaf.ui.cell;
    exports com.github.weisj.darklaf.ui.colorchooser;
    exports com.github.weisj.darklaf.ui.combobox;
    exports com.github.weisj.darklaf.ui.list;
    exports com.github.weisj.darklaf.ui.progressbar;
    exports com.github.weisj.darklaf.ui.rootpane;
    exports com.github.weisj.darklaf.ui.scrollpane;
    exports com.github.weisj.darklaf.ui.slider;
    exports com.github.weisj.darklaf.ui.spinner;
    exports com.github.weisj.darklaf.ui.splitpane;
    exports com.github.weisj.darklaf.ui.tabbedpane;
    exports com.github.weisj.darklaf.ui.table;
    exports com.github.weisj.darklaf.ui.table.renderer;
    exports com.github.weisj.darklaf.ui.text;
    exports com.github.weisj.darklaf.ui.togglebutton;
    exports com.github.weisj.darklaf.ui.toolbar;
    exports com.github.weisj.darklaf.ui.tooltip;
    exports com.github.weisj.darklaf.ui.tree;

    opens com.github.weisj.darklaf.ui.button;
    opens com.github.weisj.darklaf.ui.cell;
    opens com.github.weisj.darklaf.ui.colorchooser;
    opens com.github.weisj.darklaf.ui.combobox;
    opens com.github.weisj.darklaf.ui.filechooser;
    opens com.github.weisj.darklaf.ui.internalframe;
    opens com.github.weisj.darklaf.ui.label;
    opens com.github.weisj.darklaf.ui.list;
    opens com.github.weisj.darklaf.ui.menu;
    opens com.github.weisj.darklaf.ui.numberingpane;
    opens com.github.weisj.darklaf.ui.optionpane;
    opens com.github.weisj.darklaf.ui.panel;
    opens com.github.weisj.darklaf.ui.popupmenu;
    opens com.github.weisj.darklaf.ui.progressbar;
    opens com.github.weisj.darklaf.ui.rootpane;
    opens com.github.weisj.darklaf.ui.scrollpane;
    opens com.github.weisj.darklaf.ui.separator;
    opens com.github.weisj.darklaf.ui.slider;
    opens com.github.weisj.darklaf.ui.spinner;
    opens com.github.weisj.darklaf.ui.splitbutton;
    opens com.github.weisj.darklaf.ui.splitpane;
    opens com.github.weisj.darklaf.ui.statusbar;
    opens com.github.weisj.darklaf.ui.tabbedpane;
    opens com.github.weisj.darklaf.ui.tabframe;
    opens com.github.weisj.darklaf.ui.table;
    opens com.github.weisj.darklaf.ui.table.header;
    opens com.github.weisj.darklaf.ui.taskpane;
    opens com.github.weisj.darklaf.ui.text;
    opens com.github.weisj.darklaf.ui.titledborder;
    opens com.github.weisj.darklaf.ui.togglebutton;
    opens com.github.weisj.darklaf.ui.togglebutton.checkbox;
    opens com.github.weisj.darklaf.ui.togglebutton.radiobutton;
    opens com.github.weisj.darklaf.ui.togglebutton.tristate;
    opens com.github.weisj.darklaf.ui.toolbar;
    opens com.github.weisj.darklaf.ui.tooltip;
    opens com.github.weisj.darklaf.ui.tree;
}
