package com.weis.darklaf.ui.text;

import sun.awt.SunToolkit;
import sun.swing.DefaultLookup;
import sun.swing.SwingUtilities2;

import javax.swing.*;
import javax.swing.plaf.ActionMapUIResource;
import javax.swing.plaf.ComponentInputMapUIResource;
import javax.swing.plaf.InputMapUIResource;
import javax.swing.plaf.basic.BasicTextUI;
import javax.swing.text.Caret;
import javax.swing.text.DefaultEditorKit;
import javax.swing.text.EditorKit;
import javax.swing.text.JTextComponent;
import javax.swing.text.TextAction;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.util.HashSet;
import java.util.Set;

/**
 * @author Jannis Weis
 */
public abstract class DarkTextUI extends BasicTextUI {

    protected JTextComponent editor;

    @Override
    protected Caret createCaret() {
        return new DarkCaret(getDefaultCaretStyle());
    }

    protected abstract DarkCaret.CaretStyle getDefaultCaretStyle();

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

    protected void installKeyboardActions() {
        // backward compatibility support... keymaps for the UI
        // are now installed in the more friendly input map.
        editor.setKeymap(createKeymap());

        InputMap km = getInputMap();
        if (km != null) {
            SwingUtilities.replaceUIInputMap(editor, JComponent.WHEN_FOCUSED,
                                             km);
        }

        ActionMap map = getActionMap();
        if (map != null) {
            SwingUtilities.replaceUIActionMap(editor, map);
        }

        updateFocusAcceleratorBinding(false);
    }

    @Override
    public void installUI(final JComponent c) {
        if (c instanceof JTextComponent) {
            editor = (JTextComponent) c;
        }
        super.installUI(c);
    }



    /*
     * Implementation of BasicTextUI.
     */

    /**
     * Get the InputMap to use for the UI.
     */
    protected InputMap getInputMap() {
        InputMap map = new InputMapUIResource();

        InputMap shared =
                (InputMap) DefaultLookup.get(editor, this,
                                             getPropertyPrefix() + ".focusInputMap");
        if (shared != null) {
            map.setParent(shared);
        }
        return map;
    }

    protected ActionMap getActionMap() {
        String mapName = getPropertyPrefix() + ".actionMap";
        ActionMap map = (ActionMap) UIManager.get(mapName);

        if (map == null) {
            map = createActionMap();
            if (map != null) {
                UIManager.getLookAndFeelDefaults().put(mapName, map);
            }
        }
        ActionMap componentMap = new ActionMapUIResource();
        componentMap.put("requestFocus", new FocusAction());
        /*
         * fix for bug 4515750
         * JTextField & non-editable JTextArea bind return key - default btn not accessible
         *
         * Wrap the return action so that it is only enabled when the
         * component is editable. This allows the default button to be
         * processed when the text component has focus and isn't editable.
         *
         */
        if (getEditorKit(editor) instanceof DefaultEditorKit) {
            if (map != null) {
                Object obj = map.get(DefaultEditorKit.insertBreakAction);
                if (obj != null
                        && obj instanceof DefaultEditorKit.InsertBreakAction) {
                    Action action = new TextActionWrapper((TextAction) obj);
                    componentMap.put(action.getValue(Action.NAME), action);
                }
            }
        }
        if (map != null) {
            componentMap.setParent(map);
        }
        return componentMap;
    }

    /**
     * Invoked when the focus accelerator changes, this will update the key bindings as necessary.
     */
    @SuppressWarnings("MagicConstant")
    protected void updateFocusAcceleratorBinding(final boolean changed) {
        char accelerator = editor.getFocusAccelerator();

        if (changed || accelerator != '\0') {
            InputMap km = SwingUtilities.getUIInputMap
                    (editor, JComponent.WHEN_IN_FOCUSED_WINDOW);

            if (km == null && accelerator != '\0') {
                km = new ComponentInputMapUIResource(editor);
                SwingUtilities.replaceUIInputMap(editor, JComponent.
                        WHEN_IN_FOCUSED_WINDOW, km);
                ActionMap am = getActionMap();
                SwingUtilities.replaceUIActionMap(editor, am);
            }
            if (km != null) {
                km.clear();
                if (accelerator != '\0') {
                    km.put(KeyStroke.getKeyStroke(accelerator, getFocusAcceleratorKeyMask()), "requestFocus");
                    km.put(KeyStroke.getKeyStroke(accelerator,
                                                  SwingUtilities2.setAltGraphMask(getFocusAcceleratorKeyMask())),
                           "requestFocus");
                }
            }
        }
    }

    protected static int getFocusAcceleratorKeyMask() {
        Toolkit tk = Toolkit.getDefaultToolkit();
        if (tk instanceof SunToolkit) {
            return ((SunToolkit) tk).getFocusAcceleratorKeyMask();
        }
        return ActionEvent.ALT_MASK;
    }

    /**
     * Create a default action map.  This is basically the set of actions found exported by the component.
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

    /**
     * Invoked when editable property is changed.
     * <p>
     * removing 'TAB' and 'SHIFT-TAB' from traversalKeysSet in case editor is editable adding 'TAB' and 'SHIFT-TAB' to
     * traversalKeysSet in case editor is non editable
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

    public class FocusAction extends AbstractAction {

        public void actionPerformed(final ActionEvent e) {
            editor.requestFocus();
        }

        public boolean isEnabled() {
            return editor.isEditable();
        }
    }

    /**
     * Wrapper for text actions to return isEnabled false in case editor is non editable
     */
    public class TextActionWrapper extends TextAction {
        TextAction action;

        public TextActionWrapper(final TextAction action) {
            super((String) action.getValue(Action.NAME));
            this.action = action;
        }

        /**
         * The operation to perform when this action is triggered.
         *
         * @param e the action event
         */
        public void actionPerformed(final ActionEvent e) {
            action.actionPerformed(e);
        }

        public boolean isEnabled() {
            return (editor == null || editor.isEditable()) && action.isEnabled();
        }
    }
}
