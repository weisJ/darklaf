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
package com.github.weisj.darklaf.ui.text;

import java.awt.*;
import java.awt.event.*;
import java.beans.PropertyChangeEvent;
import java.util.Arrays;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.plaf.ComponentUI;
import javax.swing.text.JTextComponent;

import com.github.weisj.darklaf.graphics.GraphicsContext;
import com.github.weisj.darklaf.graphics.PaintUtil;
import com.github.weisj.darklaf.icons.EmptyIcon;
import com.github.weisj.darklaf.util.PropertyUtil;

/**
 * @author Konstantin Bulenkov
 * @author Jannis Weis
 */
public class DarkPasswordFieldUI extends DarkPasswordFieldUIBridge {

    protected static final String KEY_PREFIX = "JPasswordField.";
    public static final String KEY_SHOW_VIEW_BUTTON = "JPasswordField.showViewIcon";
    protected Icon show;
    protected Icon showPressed;
    protected int borderSize;
    protected int arc;
    private char echo_dot = '*';
    private boolean showTriggered = false;

    public static ComponentUI createUI(final JComponent c) {
        return new DarkPasswordFieldUI();
    }

    @Override
    protected void installDefaults() {
        super.installDefaults();
        borderSize = UIManager.getInt("PasswordField.borderThickness");
        arc = UIManager.getInt("PasswordField.arc");
        show = UIManager.getIcon("PasswordField.show.icon");
        showPressed = UIManager.getIcon("PasswordField.showPressed.icon");
    }

    protected void paintBackground(final Graphics graphics) {
        Graphics2D g = (Graphics2D) graphics;
        JTextComponent c = getComponent();

        Container parent = c.getParent();
        if (parent != null) {
            g.setColor(parent.getBackground());
            g.fillRect(0, 0, c.getWidth(), c.getHeight());
        }

        Border border = c.getBorder();
        GraphicsContext config = new GraphicsContext(g);
        if (border instanceof DarkTextBorder) {
            if (c.isEnabled() && c.isEditable()) {
                g.setColor(c.getBackground());
            }
            int width = c.getWidth();
            int height = c.getHeight();
            int w = borderSize;
            PaintUtil.fillRoundRect(g, w, w, width - 2 * w, height - 2 * w, arc);
        } else {
            super.paintBackground(g);
        }
        config.restore();
    }

    @Override
    protected boolean doPaintLeftIcon(final JTextComponent c) {
        return false;
    }

    @Override
    protected boolean doPaintRightIcon(final JTextComponent c) {
        return super.doPaintRightIcon(c) || hasShowIcon(c);
    }

    @Override
    protected Icon getLeftIcon(final JTextComponent c) {
        return EmptyIcon.create(0);
    }

    @Override
    protected Icon getRightIcon(final JTextComponent c) {
        return hasShowIcon(c) ? getShowIcon(c) : super.getRightIcon(c);
    }

    protected Icon getShowIcon(final JTextComponent c) {
        return showTriggered || !editor.isEditable() || !editor.isEditable() ? showPressed : show;
    }

    public static boolean hasShowIcon(final Component c) {
        return PropertyUtil.getBooleanProperty(c, KEY_SHOW_VIEW_BUTTON);
    }

    private boolean isOverEye(final Point p) {
        return !passwordEmpty() && DarkTextFieldUI.isOver(getRightIconCoord(), getShowIcon(editor), p);
    }

    protected boolean passwordEmpty() {
        JPasswordField c = (JPasswordField) getComponent();
        char[] pw = c.getPassword();
        boolean empty = pw.length == 0;
        Arrays.fill(pw, (char) 0);
        return empty;
    }

    @Override
    public void installUI(final JComponent c) {
        super.installUI(c);
        if (c instanceof JPasswordField) {
            echo_dot = ((JPasswordField) c).getEchoChar();
        }
    }

    @Override
    protected ClickAction getActionUnder(final Point p) {
        ClickAction action = super.getActionUnder(p);
        if (isOverEye(p)
            && hasShowIcon(editor)) {
            action = ClickAction.RIGHT_ACTION;
        }
        return action;
    }

    @Override
    public void mouseClicked(final MouseEvent e) {
        if (hasShowIcon(getComponent())) return;
        super.mouseClicked(e);
    }

    @Override
    public void mousePressed(final MouseEvent e) {
        if (hasShowIcon(editor) && isOverEye(e.getPoint())) {
            ((JPasswordField) getComponent()).setEchoChar((char) 0);
            showTriggered = true;
            getComponent().repaint();
        }
    }

    @Override
    public void mouseReleased(final MouseEvent e) {
        if (showTriggered) {
            ((JPasswordField) getComponent()).setEchoChar(echo_dot);
            showTriggered = false;
            getComponent().repaint();
        }
    }

    @Override
    public void propertyChange(final PropertyChangeEvent evt) {
        super.propertyChange(evt);
        String key = evt.getPropertyName();
        if (KEY_SHOW_VIEW_BUTTON.equals(key)) {
            editor.doLayout();
            Component parent = editor.getParent();
            if (parent instanceof JComponent) {
                parent.doLayout();
            }
            editor.repaint();
        }
    }
}
