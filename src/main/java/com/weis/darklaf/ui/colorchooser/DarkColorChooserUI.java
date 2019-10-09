package com.weis.darklaf.ui.colorchooser;

import com.weis.darklaf.color.DarkColorModel;
import com.weis.darklaf.color.DarkColorModelCMYK;
import com.weis.darklaf.color.DarkColorModelHSL;
import com.weis.darklaf.decorators.AncestorAdapter;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.colorchooser.AbstractColorChooserPanel;
import javax.swing.event.AncestorEvent;
import javax.swing.event.AncestorListener;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicColorChooserUI;
import java.awt.*;
import java.beans.PropertyChangeListener;

public class DarkColorChooserUI extends BasicColorChooserUI {

    private final PropertyChangeListener propertyChangeListener = e -> {
        if ("ancestor".equals(e.getPropertyName())) {
            var pane = (JComponent) e.getNewValue();
            if (pane != null) {
                pane = (JComponent) pane.getRootPane().getContentPane();
            } else {
                return;
            }
            var children = pane.getComponents();
            if (children.length >= 2 && children[1] instanceof JComponent) {
                var layout = ((JComponent) children[1]).getLayout();
                if (layout instanceof FlowLayout) {
                    ((FlowLayout) layout).setAlignment(FlowLayout.TRAILING);
                }
                children[1].doLayout();
            }
        }
    };
    private final AncestorListener ancestorListener = new AncestorAdapter() {
        @Override
        public void ancestorAdded(final AncestorEvent event) {
            var win = SwingUtilities.getWindowAncestor(chooser);
            if (win instanceof Dialog) {
                ((Dialog) win).setResizable(false);
                chooser.removeAncestorListener(ancestorListener);
            }
        }
    };

    @NotNull
    @Contract("_ -> new")
    public static ComponentUI createUI(final JComponent c) {
        return new DarkColorChooserUI();
    }

    @Override
    protected AbstractColorChooserPanel[] createDefaultChoosers() {
        return new AbstractColorChooserPanel[]{
                new DarkColorChooserPanel(new DarkColorModel(),
                                          new DarkColorModelHSL(),
//                                          new DarkColorModelHSB(),
                                          new DarkColorModelCMYK()),
                new DarkSwatchesChooserPanel(),
        };
    }

    @Override
    public void installUI(final JComponent c) {
        super.installUI(c);
        chooser.setPreviewPanel(new DarkPreviewPanel());
    }

    @Override
    protected void installListeners() {
        super.installListeners();
        chooser.addPropertyChangeListener(propertyChangeListener);
        chooser.addAncestorListener(ancestorListener);
    }

    @Override
    protected void uninstallListeners() {
        super.uninstallListeners();
        chooser.removePropertyChangeListener(propertyChangeListener);
        chooser.removeAncestorListener(ancestorListener);
    }
}
