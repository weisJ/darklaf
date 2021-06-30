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
package com.github.weisj.darklaf.ui.text;

import java.awt.*;
import java.awt.event.MouseEvent;
import java.beans.PropertyChangeEvent;
import java.util.Arrays;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import javax.swing.text.JTextComponent;

import com.github.weisj.darklaf.properties.icons.EmptyIcon;
import com.github.weisj.darklaf.ui.text.bridge.DarkPasswordFieldUIBridge;
import com.github.weisj.darklaf.util.PropertyUtil;

/** @author Jannis Weis */
public class DarkPasswordFieldUI extends DarkPasswordFieldUIBridge {

    protected static final String KEY_PREFIX = "JPasswordField.";
    public static final String KEY_SHOW_VIEW_BUTTON = KEY_PREFIX + "showViewIcon";
    protected Icon show;
    protected Icon showPressed;
    protected int borderSize;
    private char echo_dot = '*';
    private boolean showTriggered = false;

    public static ComponentUI createUI(final JComponent c) {
        return new DarkPasswordFieldUI();
    }

    @Override
    protected void installDefaults() {
        super.installDefaults();
        show = UIManager.getIcon("PasswordField.show.icon");
        showPressed = UIManager.getIcon("PasswordField.showPressed.icon");
    }

    @Override
    protected JPopupMenu createPopupMenu(final JTextComponent textComponent) {
        return null;
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
        return !passwordEmpty() && DarkTextFieldUI.isOver(getRightIconPos(), getShowIcon(editor), p);
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
        if (isOverEye(p) && hasShowIcon(editor)) {
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
