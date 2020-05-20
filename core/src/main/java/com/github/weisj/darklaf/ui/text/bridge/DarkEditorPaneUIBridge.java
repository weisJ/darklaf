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
package com.github.weisj.darklaf.ui.text.bridge;

import java.beans.PropertyChangeEvent;

import javax.swing.*;
import javax.swing.plaf.basic.BasicEditorPaneUI;
import javax.swing.text.DefaultEditorKit;
import javax.swing.text.EditorKit;
import javax.swing.text.JTextComponent;

import com.github.weisj.darklaf.ui.text.DarkTextUI;
import com.github.weisj.darklaf.ui.text.action.ToggleInsertAction;
import com.github.weisj.darklaf.ui.text.dummy.DummyEditorPane;
import com.github.weisj.darklaf.ui.text.dummy.DummyEditorPaneUI;
import com.github.weisj.darklaf.util.PropertyKey;
import com.github.weisj.darklaf.util.PropertyUtil;

/**
 * @author Jannis Weis
 */
public abstract class DarkEditorPaneUIBridge extends DarkTextUI {

    private static final DummyEditorPane editorPane = new DummyEditorPane();
    private static final BasicEditorPaneUI basicEditorPaneUI = new DummyEditorPaneUI();

    static {
        basicEditorPaneUI.installUI(editorPane);
    }

    @Override
    public void installUI(final JComponent c) {
        super.installUI(c);
        updateDisplayProperties(c);
    }

    @Override
    public void uninstallUI(final JComponent c) {
        cleanDisplayProperties(c);
        super.uninstallUI(c);
    }

    protected void updateDisplayProperties(final JComponent c) {
        if (c instanceof JEditorPane) {
            editorPane.setEditorPane((JEditorPane) c);
            basicEditorPaneUI.installUI(editorPane);
        }
    }

    protected void cleanDisplayProperties(final JComponent c) {
        if (c instanceof JEditorPane) {
            editorPane.setEditorPane((JEditorPane) c);
            basicEditorPaneUI.uninstallUI(editorPane);
        }
    }

    @Override
    public ActionMap getActionMap() {
        editorPane.setEditorPane((JEditorPane) getComponent());
        ActionMap am = editorPane.getActionMap();
        EditorKit editorKit = getEditorKit(getComponent());
        if (editorKit instanceof DefaultEditorKit) {
            am.put(TOGGLE_INSERT, new ToggleInsertAction());
        }
        return am;
    }

    @Override
    public void propertyChange(final PropertyChangeEvent evt) {
        super.propertyChange(evt);
        String name = evt.getPropertyName();
        if ("editorKit".equals(name)) {
            ActionMap map = SwingUtilities.getUIActionMap(getComponent());
            if (map != null) {
                Object oldValue = evt.getOldValue();
                if (oldValue instanceof EditorKit) {
                    Action[] actions = ((EditorKit) oldValue).getActions();
                    if (actions != null) {
                        removeActions(map, actions);
                    }
                }
                Object newValue = evt.getNewValue();
                if (newValue instanceof EditorKit) {
                    Action[] actions = ((EditorKit) newValue).getActions();
                    if (actions != null) {
                        addActions(map, actions);
                    }
                }
            }
            updateFocusTraversalKeys();
        } else if (PropertyKey.EDITABLE.equals(name)) {
            updateFocusTraversalKeys();
        } else if (PropertyKey.FOREGROUND.equals(name)
                   || PropertyKey.FONT.equals(name)
                   || PropertyKey.DOCUMENT.equals(name)
                   || JEditorPane.W3C_LENGTH_UNITS.equals(name)
                   || JEditorPane.HONOR_DISPLAY_PROPERTIES.equals(name)) {
            JComponent c = getComponent();
            updateDisplayProperties(getComponent());
            if (JEditorPane.W3C_LENGTH_UNITS.equals(name)
                || JEditorPane.HONOR_DISPLAY_PROPERTIES.equals(name)) {
                modelChanged();
            }
            if (PropertyKey.FOREGROUND.equals(name)) {
                if (PropertyUtil.getBooleanProperty(c, JEditorPane.HONOR_DISPLAY_PROPERTIES)) {
                    modelChanged();
                }
            }

        }
    }

    protected void addActions(final ActionMap map, final Action[] actions) {
        for (Action a : actions) {
            map.put(a.getValue(Action.NAME), a);
        }
    }

    protected void removeActions(final ActionMap map, final Action[] actions) {
        for (Action a : actions) {
            map.remove(a.getValue(Action.NAME));
        }
    }

    @Override
    public EditorKit getEditorKit(final JTextComponent tc) {
        JEditorPane pane = (JEditorPane) getComponent();
        return pane.getEditorKit();
    }
}
