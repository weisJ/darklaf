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
package com.github.weisj.darklaf.ui.splitbutton;

import java.awt.*;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;

import com.github.weisj.darklaf.components.ArrowButton;
import com.github.weisj.darklaf.components.button.JSplitButton;
import com.github.weisj.darklaf.icons.ToggleIcon;
import com.github.weisj.darklaf.ui.button.ButtonConstants;
import com.github.weisj.darklaf.ui.button.DarkButtonUI;
import com.github.weisj.darklaf.ui.popupmenu.DarkPopupMenuUI;
import com.github.weisj.darklaf.util.PropertyUtil;

public class DarkSplitButtonUI extends DarkButtonUI {

    protected JSplitButton splitButton;
    protected AbstractButton arrowButton;
    private DarkSplitButtonListener arrowButtonListener;
    private Icon overlayIcon;
    private Icon overlayDisabledIcon;

    private ToggleIcon arrowToggleIcon;
    private ToggleIcon arrowDisabledToggleIcon;

    private Insets arrowInsets;
    private Insets arrowInsetsThin;
    private final Insets arrowButtonMargin = new Insets(0, 0, 0, 0);

    public static ComponentUI createUI(final JComponent c) {
        return new DarkSplitButtonUI();
    }


    @Override
    public void installUI(final JComponent c) {
        splitButton = (JSplitButton) c;
        super.installUI(c);
        updateDefaultAction();
    }

    @Override
    public void uninstallUI(final JComponent c) {
        super.uninstallUI(c);
        splitButton = null;
    }

    @Override
    protected void installDefaults(final AbstractButton b) {
        super.installDefaults(b);
        overlayIcon = UIManager.getIcon("SplitButton.overlayIcon");
        overlayDisabledIcon = UIManager.getIcon("SplitButton.overlayDisabledIcon");
        arrowInsets = UIManager.getInsets("SplitButton.arrowInsets");
        arrowInsetsThin = UIManager.getInsets("SplitButton.arrowThinInsets");
        Icon arrowIcon = UIManager.getIcon("SplitButton.arrowIcon");
        Icon arrowIconDisabled = UIManager.getIcon("SplitButton.arrowIconDisabled");
        Icon arrowIconThin = UIManager.getIcon("SplitButton.arrowThinIcon");
        Icon arrowIconThinDisabled = UIManager.getIcon("SplitButton.arrowThinIconDisabled");

        arrowToggleIcon = new ToggleIcon(arrowIcon, arrowIconThin);
        arrowDisabledToggleIcon = new ToggleIcon(arrowIconDisabled, arrowIconThinDisabled);

        PropertyUtil.installBorder(splitButton, new DarkSplitButtonBorder());
        arrowButton = createArrowButton();
        configureArrowButton(arrowButton);
        splitButton.add(arrowButton);
        updateArrowMargin();
    }

    protected void configureArrowButton(final AbstractButton button) {
        button.setRequestFocusEnabled(false);
        button.setInheritsPopupMenu(true);
        button.resetKeyboardActions();
        button.setEnabled(splitButton.isEnabled());
        button.putClientProperty(DarkPopupMenuUI.KEY_CONSUME_EVENT_ON_CLOSE, true);
    }

    @Override
    protected void installListeners(final AbstractButton b) {
        super.installListeners(b);
        arrowButtonListener = createArrowButtonListener();
        arrowButton.addActionListener(arrowButtonListener);
        splitButton.addActionListener(arrowButtonListener);
        arrowButton.getModel().addChangeListener(arrowButtonListener);
        splitButton.addPropertyChangeListener(arrowButtonListener);
    }

    @Override
    protected void uninstallListeners(final AbstractButton b) {
        super.uninstallListeners(b);
        arrowButton.removeActionListener(arrowButtonListener);
        splitButton.removeActionListener(arrowButtonListener);
        arrowButton.getModel().removeActionListener(arrowButtonListener);
        splitButton.removePropertyChangeListener(arrowButtonListener);
        arrowButtonListener = null;
    }

    @Override
    protected void uninstallDefaults(final AbstractButton b) {
        super.uninstallDefaults(b);
        splitButton.remove(arrowButton);
        arrowButton = null;
    }

    protected void updateArrowMargin() {
        if (ButtonConstants.isThin(splitButton)) {
            arrowButtonMargin.set(arrowInsetsThin.top, arrowInsetsThin.left, arrowInsetsThin.bottom,
                    arrowInsetsThin.right);
            arrowToggleIcon.setChooseAlternativeIcon(true);
        } else {
            arrowButtonMargin.set(arrowInsets.top, arrowInsets.left, arrowInsets.bottom, arrowInsets.right);
            arrowToggleIcon.setChooseAlternativeIcon(false);
        }
        splitButton.doLayout();
    }

    protected DarkSplitButtonListener createArrowButtonListener() {
        return new DarkSplitButtonListener(this);
    }

    protected AbstractButton createArrowButton() {
        AbstractButton b = ArrowButton.createUpDownArrow(splitButton, arrowToggleIcon, arrowDisabledToggleIcon,
                SwingConstants.SOUTH, true, true, arrowButtonMargin);
        b.setRolloverEnabled(true);
        return b;
    }

    @Override
    public Dimension getPreferredSize(final JComponent c) {
        Dimension dim = super.getPreferredSize(c);
        if (useArrowButton()) {
            Dimension arrowSize = arrowButton.getPreferredSize();
            dim.width += arrowSize.width;
            dim.height = Math.max(dim.height, arrowSize.height);
        }
        return dim;
    }

    @Override
    protected LayoutManager createLayout() {
        return new DarkSplitButtonLayout();
    }

    protected boolean useArrowButton() {
        return arrowButton != null && splitButton.getActionCount() > 1;
    }

    @Override
    public void paint(final Graphics g, final JComponent c) {
        super.paint(g, c);
        g.setColor(Color.RED);

    }

    @Override
    protected void paintIcon(final Graphics g, final AbstractButton b, final JComponent c) {
        super.paintIcon(g, b, c);
        if (b.getIcon() != null && !useArrowButton()) {
            Icon overlay = b.isEnabled() ? overlayIcon : overlayDisabledIcon;
            overlay.paintIcon(c, g, iconRect.x + iconRect.width - overlay.getIconWidth(),
                    iconRect.y + iconRect.height - overlay.getIconHeight());
        }
    }

    @Override
    public boolean isRolloverBorderless(final AbstractButton b) {
        return super.isRolloverBorderless(b) || (useArrowButton() && arrowButton.getModel().isRollover());
    }

    @Override
    protected void paintDarklafBorderBgImpl(final AbstractButton c, final Graphics2D g, final boolean showShadow,
            final int shadow, final int effectiveArc, final Rectangle bgRect) {
        super.paintDarklafBorderBgImpl(c, g, showShadow, shadow, effectiveArc, bgRect);
        if (useArrowButton()) {
            boolean isDefault = splitButton.isDefaultButton();
            boolean enabled = splitButton.isEnabled();
            boolean rollover = c.isRolloverEnabled() && arrowButton.getModel().isRollover();
            boolean clicked = arrowButton.getModel().isArmed();
            g.setColor(getBackgroundColor(splitButton, isDefault, rollover, clicked, enabled));
            Shape clip = g.getClip();
            boolean ltr = c.getComponentOrientation().isLeftToRight();
            if (ltr) {
                g.clipRect(arrowButton.getX(), 0, button.getWidth(), button.getHeight());
            } else {
                g.clipRect(0, 0, arrowButton.getX() + arrowButton.getWidth(), button.getHeight());
            }
            paintBackgroundRect(g, effectiveArc, bgRect);
            g.setClip(clip);
        }
    }

    protected void setArmedClip(final AbstractButton c, final Graphics g) {
        boolean ltr = c.getComponentOrientation().isLeftToRight();
        boolean arrowArmed = arrowButton.getModel().isRollover();
        if (ltr) {
            if (arrowArmed) {
                g.clipRect(arrowButton.getX(), 0, splitButton.getWidth(), splitButton.getHeight());
            } else {
                g.clipRect(0, 0, arrowButton.getX(), splitButton.getHeight());
            }
        } else {
            if (arrowArmed) {
                g.clipRect(0, 0, arrowButton.getX() + arrowButton.getWidth(), splitButton.getHeight());
            } else {
                g.clipRect(arrowButton.getX() + arrowButton.getWidth(), 0, splitButton.getWidth(),
                        splitButton.getHeight());
            }
        }
    }

    @Override
    protected void paintBorderlessBackgroundImpl(final AbstractButton b, final Graphics2D g, final int arc, final int x,
            final int y, final int w, final int h) {
        boolean splitArmed = splitButton.getModel().isArmed();
        boolean arrowArmed = arrowButton.getModel().isArmed();
        Shape clip = g.getClip();
        if (splitArmed) {
            super.paintBorderlessBackgroundImpl(arrowButton, g, arc, x, y, w, h);
            setArmedClip(splitButton, g);
            super.paintBorderlessBackgroundImpl(splitButton, g, arc, x, y, w, h);
        } else if (arrowArmed) {
            super.paintBorderlessBackgroundImpl(splitButton, g, arc, x, y, w, h);
            setArmedClip(splitButton, g);
            super.paintBorderlessBackgroundImpl(arrowButton, g, arc, x, y, w, h);
        } else {
            super.paintBorderlessBackgroundImpl(b, g, arc, x, y, w, h);
        }
        g.setClip(clip);
    }

    @Override
    protected void paintBorderlessRectangularBackgroundIml(final AbstractButton b, final Graphics2D g, final int x,
            final int y, final int w, final int h) {
        boolean splitArmed = splitButton.getModel().isArmed();
        boolean arrowArmed = splitButton.getModel().isArmed();
        Shape clip = g.getClip();
        if (splitArmed) {
            super.paintBorderlessRectangularBackgroundIml(arrowButton, g, x, y, w, h);
            setArmedClip(splitButton, g);
            super.paintBorderlessRectangularBackgroundIml(splitButton, g, x, y, w, h);
        } else if (arrowArmed) {
            super.paintBorderlessRectangularBackgroundIml(splitButton, g, x, y, w, h);
            setArmedClip(splitButton, g);
            super.paintBorderlessRectangularBackgroundIml(arrowButton, g, x, y, w, h);
        } else {
            super.paintBorderlessRectangularBackgroundIml(b, g, x, y, w, h);
        }
        g.setClip(clip);
    }

    public void updateDefaultAction() {
        arrowButton.setVisible(useArrowButton());
        splitButton.putClientProperty(DarkPopupMenuUI.KEY_CONSUME_EVENT_ON_CLOSE, !useArrowButton());
    }

    protected class DarkSplitButtonLayout extends DarkButtonLayout {

        @Override
        public void layoutContainer(final Container parent) {
            super.layoutContainer(parent);
            if (useArrowButton()) {
                Insets ins = parent.getInsets();
                Dimension arrowSize = arrowButton.getPreferredSize();
                boolean ltr = splitButton.getComponentOrientation().isLeftToRight();
                if (ltr) {
                    arrowButton.setBounds(parent.getWidth() - ins.right - arrowSize.width, 0,
                            arrowSize.width + ins.right, parent.getHeight());
                    arrowButton.setMargin(new Insets(ins.top, 0, ins.bottom, ins.right));
                } else {
                    arrowButton.setBounds(ins.left, ins.top, arrowSize.width,
                            parent.getHeight() - ins.top - ins.bottom);
                    arrowButton.setMargin(new Insets(ins.top, ins.left, ins.bottom, 0));
                }
            }
        }

        @Override
        protected void prepareContentRects(final AbstractButton b, final int width, final int height) {
            super.prepareContentRects(b, width, height);
            if (useArrowButton()) {
                Dimension arrowSize = arrowButton.getPreferredSize();
                boolean ltr = splitButton.getComponentOrientation().isLeftToRight();
                viewRect.width -= arrowSize.width;
                if (!ltr) {
                    viewRect.x += arrowSize.width;
                }
            }
        }
    }
}
