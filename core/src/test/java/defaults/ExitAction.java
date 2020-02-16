package defaults;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;

/*
 *	Close the frame
 */
final class ExitAction extends AbstractAction {
    ExitAction() {
        putValue(Action.NAME, "Exit");
        putValue(Action.SHORT_DESCRIPTION, getValue(Action.NAME));
        putValue(Action.MNEMONIC_KEY, KeyEvent.VK_X);
    }

    public void actionPerformed(final ActionEvent e) {
        System.exit(0);
    }
}
