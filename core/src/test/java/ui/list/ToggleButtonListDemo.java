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
package ui.list;

import java.awt.*;

import javax.swing.*;

import ui.ComponentDemo;
import ui.DemoPanel;

import com.github.weisj.darklaf.components.OverlayScrollPane;
import com.github.weisj.darklaf.components.togglebuttonlist.JToggleButtonList;
import com.github.weisj.darklaf.ui.togglebutton.DarkToggleButtonUI;
import com.github.weisj.darklaf.ui.togglebutton.ToggleButtonConstants;

public class ToggleButtonListDemo implements ComponentDemo {

    public static void main(final String[] args) {
        ComponentDemo.showDemo(new ToggleButtonListDemo());
    }

    @Override
    public JComponent createComponent() {
        JToggleButtonList list = new JToggleButtonList();
        list.addToggleButton(new JCheckBox("CheckBox"));
        list.addToggleButton(new JRadioButton("RadioButton"));
        JToggleButton toggleButton = new JToggleButton("Slider ToggleButton");
        toggleButton.putClientProperty(ToggleButtonConstants.KEY_VARIANT, DarkToggleButtonUI.VARIANT_SLIDER);
        list.addToggleButton(toggleButton);
        for (int i = 0; i < 20; i++) {
            list.addToggleButton("Item " + i);
        }
        JPanel panel = new JPanel(new BorderLayout());
        panel.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
        panel.add(new OverlayScrollPane(list));
        return new DemoPanel(panel, new BorderLayout(), 0);
    }

    @Override
    public String getTitle() {
        return "Checkbox List Demo";
    }
}
