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
package ui.tabFrame;

import com.github.weisj.darklaf.components.tabframe.JTabFrame;
import com.github.weisj.darklaf.components.tabframe.TabbedPopup;
import com.github.weisj.darklaf.components.text.NonWrappingTextPane;
import com.github.weisj.darklaf.components.text.NumberedTextComponent;
import com.github.weisj.darklaf.components.text.NumberingPane;
import com.github.weisj.darklaf.icons.IconLoader;
import com.github.weisj.darklaf.util.Alignment;
import com.github.weisj.darklaf.util.StringUtil;
import ui.ComponentDemo;
import ui.DemoResources;

import javax.swing.*;
import javax.swing.text.BadLocationException;
import java.awt.*;

public class TabFrameDemo implements ComponentDemo {

    public static void main(final String[] args) {
        ComponentDemo.showDemo(new TabFrameDemo(), new Dimension(1000, 500));
    }


    private static Component createTextArea() {
        NumberedTextComponent numberPane = new NumberedTextComponent(new NonWrappingTextPane() {{
            setText(StringUtil.repeat(DemoResources.LOREM_IPSUM, 10));
            setFont(new Font(Font.MONOSPACED, Font.PLAIN, 14));
        }});
        NumberingPane numbering = numberPane.getNumberingPane();
        Icon icon = IconLoader.get().getIcon("navigation/arrowRight.svg");
        try {
            numbering.addIconAtLine(5, icon);
            numbering.addIconAtLine(10, icon);
            numbering.addIconAtLine(15, icon);
        } catch (BadLocationException e) {
            e.printStackTrace();
        }
        return numberPane;
    }

    @Override
    public JComponent createComponent() {
        Icon folderIcon = IconLoader.get().getIcon("files/folder.svg", 19, 19, true);
        JTabFrame tabFrame = new JTabFrame();
        for (Alignment o : Alignment.values()) {
            if (o != Alignment.CENTER) {
                for (int i = 0; i < 2; i++) {
                    JPanel pcc = new JPanel();
                    pcc.setOpaque(true);
                    pcc.add(new JLabel(o.toString() + "_" + i + " Popup"));
                    tabFrame.addTab(pcc, o.toString() + "_" + i, folderIcon, o);
                }
            }
        }
        TabbedPopup tabbedPopup = new TabbedPopup("Tabbed Popup:");
        tabFrame.setTabAt(tabbedPopup, "NORTH (Tabbed Pane Tab)", null, Alignment.NORTH, 0);
        for (int i = 0; i < 5; i++) {
            JPanel panel = new JPanel();
            JLabel label = new JLabel("Tab Number " + i);
            panel.add(label);
            tabbedPopup.getTabbedPane().addTab("Tab " + i, panel);
        }
            /* Activate for a custom tab.
            tabFrame.setUserTabComponentAt(new JLabel("NORTH (custom tab)") {{
                setBorder(new EmptyBorder(0, 5, 0, 5));
                setOpaque(false);
                setForeground(Color.RED);
                setFont(new Font(Font.SERIF, Font.ITALIC, 12));
            }}, Alignment.NORTH, 1);
             */
        tabFrame.setAcceleratorAt(1, Alignment.NORTH_WEST, 0);
        tabFrame.setTabEnabled(Alignment.NORTH_EAST, 0, false);

        JPanel contentPane = new JPanel(new BorderLayout());
        contentPane.add(tabFrame, BorderLayout.CENTER);
        tabFrame.setContent(createTextArea());
        return tabFrame;
    }

    @Override
    public String getTitle() {
        return "TabFrame Demo";
    }
}
