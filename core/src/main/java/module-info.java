module darklaf.core {
    exports com.github.weisj.darklaf.theme;
    exports com.github.weisj.darklaf.components;
    exports com.github.weisj.darklaf.components.alignment;
    exports com.github.weisj.darklaf.components.border;
    exports com.github.weisj.darklaf.components.tabframe;
    exports com.github.weisj.darklaf.components.text;
    exports com.github.weisj.darklaf.components.tooltip;
    exports com.github.weisj.darklaf.components.tristate;
    exports com.github.weisj.darklaf;

    requires darklaf.utils;
    requires darklaf.propertyloader;
    requires darklaf.decorationsbase;
    requires darklaf.macos;
    requires darklaf.windows;
    requires jxlayer;
    requires swingx;
    requires java.logging;
    requires java.desktop;
}
