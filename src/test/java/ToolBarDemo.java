/*
 * Copyright (c) 1995, 2008, Oracle and/or its affiliates. All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 *   - Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *
 *   - Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 *   - Neither the name of Oracle or the names of its
 *     contributors may be used to endorse or promote products derived
 *     from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
 * THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

/*
 * ToolBarDemo.java requires the following addditional files:
 * images/Back24.gif
 * images/Forward24.gif
 * images/Up24.gif
 */

import com.weis.darklaf.LafManager;
import org.jetbrains.annotations.NotNull;

import javax.swing.JToolBar;
import javax.swing.JButton;
import javax.swing.ImageIcon;

import javax.swing.JFrame;
import javax.swing.JTextArea;
import javax.swing.JScrollPane;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;

import java.net.URL;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

public class ToolBarDemo extends JPanel implements ActionListener {
    private JTextArea textArea;
    private static final String PREVIOUS = "previous";
    private static final String UP = "up";
    private static final String NEXT = "next";

    private ToolBarDemo() {
        super(new BorderLayout());

        //Create the toolbar.
        JToolBar toolBar = new JToolBar("Still draggable");
        addButtons(toolBar);

        //Create the text area used for output.  Request
        //enough space for 5 rows and 30 columns.
        textArea = new JTextArea(5, 30);
        textArea.setEditable(false);
        JScrollPane scrollPane = new JScrollPane(textArea);

        //Lay out the main panel.
        setPreferredSize(new Dimension(450, 130));
        add(toolBar, BorderLayout.PAGE_START);
        add(scrollPane, BorderLayout.CENTER);
    }

    private void addButtons(@NotNull final JToolBar toolBar) {
        JButton button;

        //first button
        button = makeNavigationButton("Back24", PREVIOUS,
                                      "Back to previous something-or-other", "Previous");
        toolBar.add(button);

        //second button
        button = makeNavigationButton("Up24", UP,
                                      "Up to something-or-other", "Up");
        toolBar.add(button);

        //third button
        button = makeNavigationButton("Forward24", NEXT,
                                      "Forward to something-or-other", "Next");
        toolBar.add(button);
    }

    @NotNull
    private JButton makeNavigationButton(final String imageName,
                                         final String actionCommand,
                                         final String toolTipText,
                                         final String altText) {
        //Look for the image.
        String imgLocation = "images/" + imageName + ".gif";
        URL imageURL = ToolBarDemo.class.getResource(imgLocation);

        //Create and initialize the button.
        JButton button = new JButton();
        button.setActionCommand(actionCommand);
        button.setToolTipText(toolTipText);
        button.addActionListener(this);

        if (imageURL != null) {                      //image found
            button.setIcon(new ImageIcon(imageURL, altText));
        } else {                                     //no image found
            button.setText(altText);
            System.err.println("Resource not found: " + imgLocation);
        }

        return button;
    }

    public void actionPerformed(@NotNull final ActionEvent e) {
        String cmd = e.getActionCommand();
        String description = null;

        // Handle each button.
        if (PREVIOUS.equals(cmd)) { //first button clicked
            description = "taken you to the previous <something>.";
        } else if (UP.equals(cmd)) { // second button clicked
            description = "taken you up one level to <something>.";
        } else if (NEXT.equals(cmd)) { // third button clicked
            description = "taken you to the next <something>.";
        }

        displayResult("If this were a real app, it would have "
                              + description);
    }

    private void displayResult(final String actionDescription) {
        String newline = "\n";
        textArea.append(actionDescription + newline);
        textArea.setCaretPosition(textArea.getDocument().getLength());
    }

    /**
     * Create the GUI and show it.  For thread safety,
     * this method should be invoked from the
     * event dispatch thread.
     */
    private static void createAndShowGUI() {
        //Create and set up the window.
        JFrame frame = new JFrame("ToolBarDemo");
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

        //Add content to the window.
        frame.add(new ToolBarDemo());

        //Display the window.
        frame.pack();
        frame.setVisible(true);
    }

    public static void main(final String[] args) {
        //Schedule a job for the event dispatch thread:
        //creating and showing this application's GUI.
        SwingUtilities.invokeLater(() -> {
            LafManager.loadLaf(LafManager.Theme.Dark);
            //Turn off metal's use of bold fonts
            UIManager.put("swing.boldMetal", Boolean.FALSE);
            createAndShowGUI();
        });
    }
}