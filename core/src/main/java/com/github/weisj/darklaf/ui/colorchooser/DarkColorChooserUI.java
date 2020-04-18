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
package com.github.weisj.darklaf.ui.colorchooser;

import java.awt.*;
import java.beans.PropertyChangeListener;

import javax.swing.*;
import javax.swing.colorchooser.AbstractColorChooserPanel;
import javax.swing.event.AncestorEvent;
import javax.swing.event.AncestorListener;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicColorChooserUI;

import com.github.weisj.darklaf.color.DarkColorModelCMYK;
import com.github.weisj.darklaf.color.DarkColorModelHSB;
import com.github.weisj.darklaf.color.DarkColorModelHSL;
import com.github.weisj.darklaf.color.DarkColorModelRGB;
import com.github.weisj.darklaf.decorators.AncestorAdapter;
import com.github.weisj.darklaf.util.PropertyKey;

/**
 * @author Jannis Weis
 */
public class DarkColorChooserUI extends BasicColorChooserUI {

    private final PropertyChangeListener propertyChangeListener = e -> {
        if (PropertyKey.ANCESTOR.equals(e.getPropertyName())) {
            JComponent pane = (JComponent) e.getNewValue();
            if (pane != null) {
                pane = (JComponent) pane.getRootPane().getContentPane();
            } else {
                return;
            }
            Component[] children = pane.getComponents();
            if (children.length >= 2 && children[1] instanceof JComponent) {
                LayoutManager layout = ((JComponent) children[1]).getLayout();
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
            Window win = SwingUtilities.getWindowAncestor(chooser);
            if (win instanceof Dialog) {
                ((Dialog) win).setResizable(false);
                chooser.removeAncestorListener(ancestorListener);
            }
        }
    };

    public static ComponentUI createUI(final JComponent c) {
        return new DarkColorChooserUI();
    }

    @Override
    protected AbstractColorChooserPanel[] createDefaultChoosers() {
        return new AbstractColorChooserPanel[]{
                                               new DarkColorChooserPanel(DarkColorModelRGB.getInstance(),
                                                                         DarkColorModelHSB.getInstance(),
                                                                         DarkColorModelHSL.getInstance(),
                                                                         DarkColorModelCMYK.getInstance()),
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
