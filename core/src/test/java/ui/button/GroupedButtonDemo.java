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
package ui.button;

import com.github.weisj.darklaf.icons.IconLoader;
import com.github.weisj.darklaf.ui.button.DarkButtonUI;
import com.github.weisj.darklaf.util.AlignmentExt;
import ui.ComponentDemo;
import ui.DemoPanel;

import javax.swing.*;

public class GroupedButtonDemo implements ComponentDemo {

    public static void main(final String[] args) {
        ComponentDemo.showDemo(new GroupedButtonDemo());
    }

    @Override
    public JComponent createComponent() {
        Box box = new Box(BoxLayout.LINE_AXIS);
        Icon icon = IconLoader.get().getIcon("menu/listFiles.svg", 19, 19, true);
        Icon iconSelected = IconLoader.get().getIcon("menu/listFilesSelected.svg", 19, 19, true);
        ButtonGroup bg = new ButtonGroup();
        box.add(createButton(icon, iconSelected, bg, AlignmentExt.LEFT));
        for (int i = 0; i < 3; i++) {
            box.add(createButton(icon, iconSelected, bg, AlignmentExt.MIDDLE_HORIZONTAL));
        }
        box.add(createButton(icon, iconSelected, bg, AlignmentExt.RIGHT, true));
        return new DemoPanel(box);
    }

    protected AbstractButton createButton(final Icon icon, final Icon selected,
                                          final ButtonGroup bg, final AlignmentExt a) {
        return createButton(icon, selected, bg, a, false);
    }

    protected AbstractButton createButton(final Icon icon, final Icon selected,
                                          final ButtonGroup bg, final AlignmentExt a,
                                          final boolean isSelected) {
        return new JToggleButton(icon) {{
            setSelectedIcon(selected);
            putClientProperty(DarkButtonUI.KEY_THIN, true);
            putClientProperty(DarkButtonUI.KEY_CORNER, a);
            setSelected(isSelected);
//            bg.add(this);
        }};
    }

    @Override
    public String getTitle() {
        return "Grouped Button Demo";
    }
}
