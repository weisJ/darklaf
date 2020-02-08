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

package demo.dialog;

import javax.swing.*;
import javax.swing.text.DefaultFormatterFactory;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.text.ParseException;

/* 1.4 example used by DialogDemo.java. */
class CustomDialog extends JDialog implements ActionListener, PropertyChangeListener {
    private String typedText = null;
    private JFormattedTextField textField;
    private DialogDemo dd;

    private String magicWord;
    private JOptionPane optionPane;

    private String btnString1 = "Enter";
    private String btnString2 = "Cancel";

    /**
     * Creates the reusable demo.dialog.
     */
    public CustomDialog(final Frame aFrame, final String aWord, final DialogDemo parent) {
        super(aFrame, true);
        dd = parent;

        magicWord = aWord.toUpperCase();
        setTitle("Quiz");

        textField = new JFormattedTextField("");
        textField.setColumns(10);
        textField.setFormatterFactory(new DefaultFormatterFactory(new JFormattedTextField.AbstractFormatter() {
            @Override
            public Object stringToValue(final String text) throws ParseException {
                if (!magicWord.equals(text)) {
                    throw new ParseException("Invalid magic word", 0);
                }
                return text;
            }

            @Override
            public String valueToString(final Object value) {
                return value == null ? "" : value.toString();
            }
        }));
        textField.setFocusLostBehavior(JFormattedTextField.COMMIT);

        //Create an array of the text and components to be displayed.
        String msgString1 = "What was Dr. SEUSS's real last name?";
        String msgString2 = "(The answer is \"" + magicWord + "\".)";
        Object[] array = {msgString1, msgString2, textField};

        //Create an array specifying the number of demo.dialog buttons
        //and their text.
        Object[] options = {btnString1, btnString2};

        //Create the JOptionPane.
        optionPane = new JOptionPane(array, JOptionPane.QUESTION_MESSAGE, JOptionPane.YES_NO_OPTION,
                                     null, options, options[0]);

        //Make this demo.dialog display it.
        setContentPane(optionPane);

        //Handle window closing correctly.
        setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
        addWindowListener(new WindowAdapter() {
            public void windowClosing(final WindowEvent we) {
                /*
                 * Instead of directly closing the window,
                 * we're going to change the JOptionPane's
                 * value property.
                 */
                optionPane.setValue(JOptionPane.CLOSED_OPTION);
            }
        });

        //Ensure the text field always gets the first focus.
        addComponentListener(new ComponentAdapter() {
            public void componentShown(final ComponentEvent ce) {
                textField.requestFocusInWindow();
            }
        });

        //Register an event handler that puts the text into the option pane.
        textField.addActionListener(this);

        //Register an event handler that reacts to option pane state changes.
        optionPane.addPropertyChangeListener(this);
    }

    /**
     * Returns null if the typed string was invalid; otherwise, returns the string as the user entered it.
     */
    public String getValidatedText() {
        return typedText;
    }

    /**
     * This method handles events for the text field.
     */
    public void actionPerformed(final ActionEvent e) {
        optionPane.setValue(btnString1);
    }

    /**
     * This method reacts to state changes in the option pane.
     */
    public void propertyChange(final PropertyChangeEvent e) {
        String prop = e.getPropertyName();

        if (isVisible()
                && (e.getSource() == optionPane)
                && (JOptionPane.VALUE_PROPERTY.equals(prop) ||
                JOptionPane.INPUT_VALUE_PROPERTY.equals(prop))) {
            Object value = optionPane.getValue();

            if (value == JOptionPane.UNINITIALIZED_VALUE) {
                //ignore reset
                return;
            }

            //Reset the JOptionPane's value.
            //If you don't do this, then if the user
            //presses the same button next time, no
            //property change event will be fired.
            optionPane.setValue(JOptionPane.UNINITIALIZED_VALUE);

            if (btnString1.equals(value)) {
                typedText = textField.getText();
                String ucText = typedText.toUpperCase();
                if (magicWord.equals(ucText)) {
                    //we're done; clear and dismiss the demo.dialog
                    clearAndHide();
                } else {
                    //text was invalid
                    textField.selectAll();
                    JOptionPane.showMessageDialog(CustomDialog.this,
                                                  "Sorry, \"" + typedText + "\" " + "isn't a valid response.\n"
                                                          + "Please enter " + magicWord + ".",
                                                  "Try again", JOptionPane.ERROR_MESSAGE);
                    typedText = null;
                    textField.requestFocusInWindow();
                }
            } else { //user closed demo.dialog or clicked cancel
                dd.setLabel("It's OK.   We won't force you to type " + magicWord + ".");
                typedText = null;
                clearAndHide();
            }
        }
    }

    /**
     * This method clears the demo.dialog and hides it.
     */
    public void clearAndHide() {
        textField.setText(null);
        setVisible(false);
    }
}
