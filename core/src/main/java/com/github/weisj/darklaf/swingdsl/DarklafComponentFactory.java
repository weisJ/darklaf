package com.github.weisj.darklaf.swingdsl;

import com.github.weisj.darklaf.components.OverlayScrollPane;
import com.github.weisj.darklaf.components.border.DarkBorders;
import com.github.weisj.swingdsl.laf.DefaultComponentFactory;
import com.github.weisj.swingdsl.laf.DefaultWrappedComponent;
import com.github.weisj.swingdsl.laf.WrappedComponent;

import javax.swing.BorderFactory;
import javax.swing.JComponent;
import javax.swing.JScrollPane;
import javax.swing.border.Border;

public class DarklafComponentFactory extends DefaultComponentFactory {
    @Override
    public WrappedComponent<JScrollPane> createScrollPane(final JComponent content) {
        OverlayScrollPane sp = new OverlayScrollPane(content);
        return new DefaultWrappedComponent<>(sp.getScrollPane(), sp);
    }

    @Override
    public Border createDividerBorder(final String title) {
        return BorderFactory.createTitledBorder(DarkBorders.createTopBorder(), title);
    }
}
