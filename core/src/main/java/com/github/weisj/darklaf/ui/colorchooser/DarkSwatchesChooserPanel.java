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
 */
package com.github.weisj.darklaf.ui.colorchooser;

import javax.accessibility.AccessibleContext;
import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.border.EmptyBorder;
import javax.swing.border.LineBorder;
import javax.swing.colorchooser.AbstractColorChooserPanel;
import javax.swing.colorchooser.ColorSelectionModel;
import java.awt.*;
import java.awt.event.*;
import java.io.Serializable;

/**
 * @author Jannis Weis
 */
public class DarkSwatchesChooserPanel extends AbstractColorChooserPanel {


    private SwatchPanel swatchPanel;
    private RecentSwatchPanel recentSwatchPanel;
    private ColorPreviewComponent previewPanel;
    private MouseListener mainSwatchListener;
    private MouseListener recentSwatchListener;
    private KeyListener mainSwatchKeyListener;
    private KeyListener recentSwatchKeyListener;

    public DarkSwatchesChooserPanel() {
        setInheritsPopupMenu(true);
    }

    @Override
    public void updateChooser() {

    }

    @Override
    protected void buildChooser() {
        String recentStr = UIManager.getString("ColorChooser.swatchesRecentText", getLocale());

        JPanel superHolder = new JPanel();
        superHolder.setLayout(new BorderLayout());

        previewPanel = new ColorPreviewComponent();
        previewPanel.setColor(getColorFromModel());

        swatchPanel = new MainSwatchPanel();
        swatchPanel.putClientProperty(AccessibleContext.ACCESSIBLE_NAME_PROPERTY, getDisplayName());
        swatchPanel.setInheritsPopupMenu(true);

        recentSwatchPanel = new RecentSwatchPanel();
        recentSwatchPanel.putClientProperty(AccessibleContext.ACCESSIBLE_NAME_PROPERTY, recentStr);

        mainSwatchKeyListener = new MainSwatchKeyListener();
        mainSwatchListener = new MainSwatchListener();
        swatchPanel.addMouseListener(mainSwatchListener);
        swatchPanel.addKeyListener(mainSwatchKeyListener);
        recentSwatchListener = new RecentSwatchListener();
        recentSwatchKeyListener = new RecentSwatchKeyListener();
        recentSwatchPanel.addMouseListener(recentSwatchListener);
        recentSwatchPanel.addKeyListener(recentSwatchKeyListener);

        JPanel mainHolder = new JPanel(new FlowLayout(FlowLayout.CENTER));
        Border border = new LineBorder(UIManager.getColor("ColorChooser.swatchBorderColor"));
        swatchPanel.setBorder(border);
        mainHolder.add(swatchPanel);

        recentSwatchPanel.setInheritsPopupMenu(true);
        JPanel recentHolder = new JPanel(new FlowLayout(FlowLayout.CENTER));
        recentSwatchPanel.setBorder(border);
        recentHolder.setInheritsPopupMenu(true);
        recentHolder.add(recentSwatchPanel);

        JLabel l = new JLabel(recentStr);
        l.setLabelFor(recentSwatchPanel);
        JPanel labelHolder = new JPanel(new FlowLayout(FlowLayout.LEFT));
        labelHolder.add(l);

        JPanel previewHolder = new JPanel(new BorderLayout());
        previewHolder.setBorder(new EmptyBorder(0, 5, 10, 5));
        previewHolder.add(previewPanel, BorderLayout.CENTER);

        JPanel swatches = new JPanel();
        swatches.setInheritsPopupMenu(true);
        swatches.setLayout(new BoxLayout(swatches, BoxLayout.Y_AXIS));
        swatches.add(mainHolder);
        swatches.add(labelHolder);
        swatches.add(recentHolder);

        superHolder.add(previewHolder, BorderLayout.NORTH);
        superHolder.add(swatches, BorderLayout.CENTER);
        superHolder.setInheritsPopupMenu(true);

        add(superHolder);
    }

    public String getDisplayName() {
        return UIManager.getString("ColorChooser.swatchesNameText", getLocale());
    }

    @Override
    public Icon getSmallDisplayIcon() {
        return null;
    }

    @Override
    public Icon getLargeDisplayIcon() {
        return null;
    }

    public void installChooserPanel(final JColorChooser enclosingChooser) {
        super.installChooserPanel(enclosingChooser);
    }

    public void uninstallChooserPanel(final JColorChooser enclosingChooser) {
        super.uninstallChooserPanel(enclosingChooser);
        swatchPanel.removeMouseListener(mainSwatchListener);
        swatchPanel.removeKeyListener(mainSwatchKeyListener);
        recentSwatchPanel.removeMouseListener(recentSwatchListener);
        recentSwatchPanel.removeKeyListener(recentSwatchKeyListener);

        swatchPanel = null;
        recentSwatchPanel = null;
        mainSwatchListener = null;
        mainSwatchKeyListener = null;
        recentSwatchListener = null;
        recentSwatchKeyListener = null;

        removeAll();  // strip out all the sub-components
    }

    protected void setSelectedColor(final Color color) {
        ColorSelectionModel model = getColorSelectionModel();
        previewPanel.setColor(color);
        if (model != null) {
            model.setSelectedColor(color);
        }
    }

    protected class RecentSwatchKeyListener extends KeyAdapter {
        public void keyPressed(final KeyEvent e) {
            if (KeyEvent.VK_SPACE == e.getKeyCode()) {
                Color color = recentSwatchPanel.getSelectedColor();
                setSelectedColor(color);
            }
        }
    }

    protected class MainSwatchKeyListener extends KeyAdapter {
        public void keyPressed(final KeyEvent e) {
            if (KeyEvent.VK_SPACE == e.getKeyCode()) {
                Color color = swatchPanel.getSelectedColor();
                setSelectedColor(color);
                recentSwatchPanel.setMostRecentColor(color);
            }
        }
    }

    protected class RecentSwatchListener extends MouseAdapter implements Serializable {
        public void mousePressed(final MouseEvent e) {
            if (isEnabled()) {
                Color color = recentSwatchPanel.getColorForLocation(e.getX(), e.getY());
                recentSwatchPanel.setSelectedColorFromLocation(e.getX(), e.getY());
                setSelectedColor(color);
                recentSwatchPanel.requestFocusInWindow();
            }
        }
    }

    protected class MainSwatchListener extends MouseAdapter implements Serializable {
        public void mousePressed(final MouseEvent e) {
            if (isEnabled()) {
                Color color = swatchPanel.getColorForLocation(e.getX(), e.getY());
                setSelectedColor(color);
                swatchPanel.setSelectedColorFromLocation(e.getX(), e.getY());
                recentSwatchPanel.setMostRecentColor(color);
                swatchPanel.requestFocusInWindow();
            }
        }
    }

}
