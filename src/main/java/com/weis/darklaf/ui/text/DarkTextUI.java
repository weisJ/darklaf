package com.weis.darklaf.ui.text;

import javax.swing.*;
import javax.swing.plaf.ActionMapUIResource;
import javax.swing.plaf.basic.BasicTextUI;
import javax.swing.text.Caret;
import javax.swing.text.DefaultEditorKit;
import javax.swing.text.EditorKit;
import javax.swing.text.JTextComponent;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.util.HashSet;
import java.util.Set;

public abstract class DarkTextUI extends BasicTextUI {

    protected JTextComponent editor;

    @Override
    protected Caret createCaret() {
        return new DarkCaret(getDefaultCaretStyle());
    }

    protected abstract DarkCaret.CaretStyle getDefaultCaretStyle();

    @Override
    public void installUI(final JComponent c) {
        if (c instanceof JTextComponent) {
            editor = (JTextComponent) c;
        }
        super.installUI(c);
    }

    @Override
    protected void installDefaults() {
        super.installDefaults();
        editor.putClientProperty("JTextComponent.roundedSelection",
                                 UIManager.getBoolean("TextComponent.roundedSelection"));
    }

    @Override
    protected void uninstallDefaults() {
        super.uninstallDefaults();
        editor.putClientProperty("JTextComponent.roundedSelection", null);
    }

    public class FocusAction extends AbstractAction {

        public void actionPerformed(final ActionEvent e) {
            editor.requestFocus();
        }

        public boolean isEnabled() {
            return editor.isEditable();
        }
    }



    /*
     * Implementation of BasicTextUI.
     */

    /**
     * Invoked when editable property is changed.
     * <p>
     * removing 'TAB' and 'SHIFT-TAB' from traversalKeysSet in case
     * editor is editable
     * adding 'TAB' and 'SHIFT-TAB' to traversalKeysSet in case
     * editor is non editable
     */
    @SuppressWarnings("deprecation")
    void updateFocusTraversalKeys() {
        /*
         * Fix for 4514331 Non-editable JTextArea and similar
         * should allow Tab to keyboard - accessibility
         */
        EditorKit editorKit = getEditorKit(editor);
        if (editorKit instanceof DefaultEditorKit) {
            Set<AWTKeyStroke> storedForwardTraversalKeys =
                    editor.getFocusTraversalKeys(KeyboardFocusManager.FORWARD_TRAVERSAL_KEYS);
            Set<AWTKeyStroke> storedBackwardTraversalKeys =
                    editor.getFocusTraversalKeys(KeyboardFocusManager.BACKWARD_TRAVERSAL_KEYS);
            Set<AWTKeyStroke> forwardTraversalKeys =
                    new HashSet<>(storedForwardTraversalKeys);
            Set<AWTKeyStroke> backwardTraversalKeys =
                    new HashSet<>(storedBackwardTraversalKeys);
            if (editor.isEditable()) {
                forwardTraversalKeys.remove(KeyStroke.getKeyStroke(KeyEvent.VK_TAB, 0));
                backwardTraversalKeys.remove(KeyStroke.getKeyStroke(KeyEvent.VK_TAB, InputEvent.SHIFT_MASK));
            } else {
                forwardTraversalKeys.add(KeyStroke.getKeyStroke(KeyEvent.VK_TAB, 0));
                backwardTraversalKeys.add(KeyStroke.getKeyStroke(KeyEvent.VK_TAB, InputEvent.SHIFT_MASK));
            }
            LookAndFeel.installProperty(editor, "focusTraversalKeysForward", forwardTraversalKeys);
            LookAndFeel.installProperty(editor, "focusTraversalKeysBackward", backwardTraversalKeys);
        }
    }

    /**
     * Create a default action map.  This is basically the
     * set of actions found exported by the component.
     */
    public ActionMap createActionMap() {
        ActionMap map = new ActionMapUIResource();
        Action[] actions = editor.getActions();
        for (Action a : actions) {
            map.put(a.getValue(Action.NAME), a);
        }
        map.put(TransferHandler.getCutAction().getValue(Action.NAME),
                TransferHandler.getCutAction());
        map.put(TransferHandler.getCopyAction().getValue(Action.NAME),
                TransferHandler.getCopyAction());
        map.put(TransferHandler.getPasteAction().getValue(Action.NAME),
                TransferHandler.getPasteAction());
        return map;
    }
}
