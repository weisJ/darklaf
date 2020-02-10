package defaults;

import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.event.ActionEvent;

/*
 *  Change the LAF and recreate the defaults.UIManagerDefaults so that the properties
 *  of the new LAF are correctly displayed.
 */
final class ChangeLookAndFeelAction extends AbstractAction {
    private final UIManagerDefaults defaults;
    private final String laf;

    ChangeLookAndFeelAction(final UIManagerDefaults defaults, final String laf, final String name) {
        this.defaults = defaults;
        this.laf = laf;
        putValue(Action.NAME, name);
        putValue(Action.SHORT_DESCRIPTION, getValue(Action.NAME));
    }

    public void actionPerformed(@NotNull final ActionEvent e) {
        try {
            UIManager.setLookAndFeel(laf);
            defaults.resetComponents();

            final JRootPane rootPane = SwingUtilities.getRootPane(defaults.getContentPane());
            SwingUtilities.updateComponentTreeUI(rootPane);
            //  Use custom decorations when supported by the LAF
            final JFrame frame = (JFrame) SwingUtilities.windowForComponent(rootPane);
            frame.dispose();

            if (UIManager.getLookAndFeel().getSupportsWindowDecorations()) {
                frame.setUndecorated(true);
                frame.getRootPane().setWindowDecorationStyle(JRootPane.FRAME);
            } else {
                frame.setUndecorated(false);
            }
            frame.setVisible(true);
        } catch (@NotNull final Exception ex) {
            ex.printStackTrace();
        }
    }
}
