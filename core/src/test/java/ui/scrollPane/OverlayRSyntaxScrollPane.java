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
package ui.scrollPane;

import java.awt.*;

import javax.swing.*;

import org.fife.ui.rsyntaxtextarea.RSyntaxTextArea;
import org.fife.ui.rtextarea.RTextScrollPane;

import ui.ComponentDemo;
import ui.DemoPanel;
import ui.DemoResources;

import com.github.weisj.darklaf.components.OverlayScrollPane;
import com.github.weisj.darklaf.util.StringUtil;

public class OverlayRSyntaxScrollPane implements ComponentDemo {

    public static void main(final String[] args) {
        ComponentDemo.showDemo(new OverlayRSyntaxScrollPane());
    }

    @Override
    public JComponent createComponent() {
        RSyntaxTextArea textArea = new RSyntaxTextArea(StringUtil.repeat(DemoResources.LOREM_IPSUM, 5));
        RTextScrollPane sp = new RTextScrollPane(textArea);
        OverlayScrollPane scrollPane = new OverlayScrollPane(sp);
        return new DemoPanel(scrollPane, new BorderLayout(), 0);
    }

    @Override
    public String getTitle() {
        return "OverlayRSyntaxScrollPane Demo";
    }
}
