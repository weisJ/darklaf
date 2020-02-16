import com.github.weisj.darklaf.LafManager;
import com.github.weisj.darklaf.components.text.SearchTextField;
import com.github.weisj.darklaf.components.text.SearchTextFieldWithHistory;
import com.github.weisj.darklaf.components.tristate.TristateCheckBox;
import com.github.weisj.darklaf.icons.IconLoader;
import org.jdesktop.swingx.JXStatusBar;
import org.jdesktop.swingx.JXTaskPane;
import org.jdesktop.swingx.JXTaskPaneContainer;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.util.logging.Logger;

/**
 * @author Jannis Weis
 * @since 2018
 */
public final class UIDemo {

    public static void main(final String[] args) {
        System.setProperty("org.apache.batik.warn_destination", "false");
        SwingUtilities.invokeLater(
                () -> {
                    LafManager.install();
                    JFrame.setDefaultLookAndFeelDecorated(true);

                    JXTaskPaneContainer taskpanecontainer = new JXTaskPaneContainer();
                    JXTaskPane taskpane = new JXTaskPane();
                    for (int i = 0; i < 3; i++) {
                        taskpane.add(new AbstractAction("Test Task " + i) {
                            @Override
                            public void actionPerformed(final ActionEvent e) {
                                Logger.getGlobal().info("hello from test task");
                            }
                        });
                    }
                    taskpane.setTitle("My Tasks");
                    taskpanecontainer.add(taskpane);

                    JFrame frame = new JFrame("UIDemo");
                    frame.setIconImage(Toolkit.getDefaultToolkit().createImage(
                            UIDemo.class.getClassLoader().getResource("mima.png")
                    ));

                    Icon folderIcon = IconLoader.get().getIcon("files/folder.svg", 19, 19, true);

                    JPanel panel = new JPanel(new GridLayout(3, 4));
                    JPanel content = new JPanel(new BorderLayout());
                    content.add(panel, BorderLayout.CENTER);
                    JXStatusBar statusBar = new JXStatusBar();
                    statusBar.add(new JLabel("test1"));
                    statusBar.add(new JLabel("test2"));
                    statusBar.add(new JLabel("test3"));
                    content.add(statusBar, BorderLayout.SOUTH);

                    JButton defaultButton = new JButton("default") {{
                        setDefaultCapable(true);
                    }};

                    panel.add(new JPanel() {{
                        setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
                        add(new JPanel(new FlowLayout(FlowLayout.LEFT)) {{
                            add(new JCheckBox("disabled selected") {{
                                setSelected(true);
                                setEnabled(false);
                            }});
                            add(new JCheckBox("enabled"));
                        }});
                        add(new JPanel(new FlowLayout(FlowLayout.LEFT)) {{
                            add(new JRadioButton("disabled") {{
                                setSelected(true);
                                setEnabled(false);
                            }});
                            add(new JRadioButton("enabled"));
                        }});
                        add(new JPanel(new FlowLayout(FlowLayout.LEFT)) {{
                            add(new TristateCheckBox("disabled") {{
                                setIndeterminate();
                                setEnabled(false);
                            }});
                            add(new TristateCheckBox("enabled"));
                        }});
                        add(new JPanel(new FlowLayout(FlowLayout.LEFT)) {{
                            add(new JButton("IconButton", folderIcon) {{
                                setRolloverEnabled(true);
                                putClientProperty("JButton.variant", "shadow");
                            }});
                            add(new JButton(folderIcon) {{
                                setRolloverEnabled(true);
                                putClientProperty("JButton.square", true);
                                putClientProperty("JButton.variant", "shadow");
                            }});
                            add(new JButton(folderIcon) {{
                                setRolloverEnabled(true);
                                putClientProperty("JButton.square", true);
                                putClientProperty("JButton.thin", Boolean.TRUE);
                                putClientProperty("JButton.alternativeArc", Boolean.TRUE);
                                putClientProperty("JButton.variant", "shadow");
                            }});
                            add(new JButton(folderIcon) {{
                                putClientProperty("JButton.variant", "onlyLabel");
                            }});
                        }});
                        add(new JToggleButton("toggle") {{
                            putClientProperty("JToggleButton.variant", "slider");
                            setEnabled(false);
                            setSelected(true);
                        }});
                    }});
                    panel.add(new JPanel() {{
                        setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
                        add(new JComboBox<String>() {{
                            addItem("Editable ComboBox");
                            for (int i = 0; i < 20; i++) {
                                addItem("item " + i);
                            }
                            setEditable(true);
                        }});
                        add(new JComboBox<String>() {{
                            addItem("Uneditable ComboBox");
                            for (int i = 0; i < 20; i++) {
                                addItem("item " + i);
                            }
                            setComponentOrientation(ComponentOrientation.RIGHT_TO_LEFT);
                        }});
                        add(new JComboBox<String>() {{
                            addItem("DisabledComboBox");
                            setEnabled(false);
                        }});
                        add(new JSpinner() {{
                            putClientProperty("JSpinner.variant", "plusMinus");
                        }});
                        add(new JSpinner() {{
                            setComponentOrientation(ComponentOrientation.RIGHT_TO_LEFT);
                        }});
                        add(new JSpinner() {{
                            setEnabled(false);
                        }});
                    }});
                    panel.add(new JPanel() {{
                        setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
                        add(new JTextField("TextField"));
                        add(new JTextField("TextField") {{
                            setEnabled(false);
                        }});
                        add(new JTextField("TextField") {{
                            putClientProperty("JTextField.alternativeArc", Boolean.TRUE);
                        }});
                        add(new SearchTextField("SearchField"));
                        add(new SearchTextFieldWithHistory("SearchFieldWithHistory"));
                        add(new JPasswordField("Password"));
                        add(new JPasswordField("VeryStrongPassword") {{
                            putClientProperty("JTextField.alternativeArc", Boolean.TRUE);
                            putClientProperty("JPasswordField.showViewIcon", Boolean.TRUE);
                        }});
                    }});
                    panel.add(new JPanel() {{
                        setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
                        add(new JButton("enabled") {{
                        }});
                        add(new JButton("disabled") {{
                            setEnabled(false);
                        }});
                        add(defaultButton);
                        add(new JToggleButton("toggle") {{
                            putClientProperty("JToggleButton.variant", "slider");
                        }});
                        add(new JButton("square") {{
                            putClientProperty("JButton.square", true);
                        }});
                    }});
                    panel.add(taskpanecontainer);
                    panel.add(new JPanel() {{
                        add(new JProgressBar() {{
                            setValue(50);
                        }});
                        add(new JProgressBar() {{
                            setValue(50);
                            putClientProperty("JProgressBar.failed", true);
                        }});
                        add(new JProgressBar() {{
                            setValue(50);
                            putClientProperty("JProgressBar.passed", true);
                        }});
                        add(new JProgressBar() {{
                            setIndeterminate(true);
                        }});
                        add(new JProgressBar() {{
                            setIndeterminate(true);
                            putClientProperty("JProgressBar.failed", true);
                        }});
                        add(new JProgressBar() {{
                            setIndeterminate(true);
                            putClientProperty("JProgressBar.passed", true);
                        }});
                    }});
                    panel.add(new JPanel() {{
                        setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
                        add(new JSlider());
                        add(new JSlider() {{
                            setComponentOrientation(ComponentOrientation.RIGHT_TO_LEFT);
                        }});
                        add(new JSlider() {{
                            setInverted(true);
                        }});
                        add(new JSlider() {{
                            setInverted(true);
                            setComponentOrientation(ComponentOrientation.RIGHT_TO_LEFT);
                        }});
                        add(new JSlider() {{
                            putClientProperty("Slider.variant", "volume");
                            putClientProperty("Slider.instantScrollEnabled", Boolean.TRUE);
                        }});
                        add(new JSlider() {{
                            putClientProperty("Slider.variant", "volume");
                            putClientProperty("Slider.instantScrollEnabled", Boolean.TRUE);
                            putClientProperty("Slider.volume.showIcon", Boolean.TRUE);
                        }});
                        add(new JSlider() {{
                            setComponentOrientation(ComponentOrientation.RIGHT_TO_LEFT);
                            putClientProperty("Slider.variant", "volume");
                            putClientProperty("Slider.instantScrollEnabled", Boolean.TRUE);
                            putClientProperty("Slider.volume.showIcon", Boolean.TRUE);
                        }});
                    }});
                    panel.add(new JPanel() {{
                        setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
                        add(new JSlider() {{
                            setSnapToTicks(true);
                            setPaintTicks(true);
                            setMajorTickSpacing(20);
                            setMinorTickSpacing(5);
                            setPaintLabels(true);
                        }});
                        add(new JSlider() {{
                            setSnapToTicks(true);
                            setPaintTicks(true);
                            setMajorTickSpacing(20);
                            setMinorTickSpacing(5);
                            setPaintLabels(true);
                            setComponentOrientation(ComponentOrientation.RIGHT_TO_LEFT);
                        }});
                        add(new JSlider() {{
                            setSnapToTicks(true);
                            setPaintTicks(true);
                            setMajorTickSpacing(20);
                            setMinorTickSpacing(5);
                            setPaintLabels(true);
                            setInverted(true);
                        }});
                        add(new JSlider() {{
                            setSnapToTicks(true);
                            setPaintTicks(true);
                            setMajorTickSpacing(20);
                            setMinorTickSpacing(5);
                            setPaintLabels(true);
                            setInverted(true);
                            setComponentOrientation(ComponentOrientation.RIGHT_TO_LEFT);
                        }});
                    }});
                    panel.add(new JPanel() {{
                        setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
                        add(new JSlider() {{
                            setSnapToTicks(true);
                            setPaintTicks(true);
                            setMajorTickSpacing(20);
                            setMinorTickSpacing(5);
                            setPaintLabels(true);
                            putClientProperty("Slider.variant", "volume");
                            putClientProperty("Slider.instantScrollEnabled", Boolean.TRUE);
                        }});
                        add(new JSlider() {{
                            setSnapToTicks(true);
                            setPaintTicks(true);
                            setMajorTickSpacing(20);
                            setMinorTickSpacing(5);
                            setPaintLabels(true);
                            putClientProperty("Slider.variant", "volume");
                            putClientProperty("Slider.instantScrollEnabled", Boolean.TRUE);
                            putClientProperty("Slider.volume.showIcon", Boolean.TRUE);
                        }});
                        add(new JSlider() {{
                            setSnapToTicks(true);
                            setPaintTicks(true);
                            setMajorTickSpacing(20);
                            setMinorTickSpacing(5);
                            setPaintLabels(true);
                            setComponentOrientation(ComponentOrientation.RIGHT_TO_LEFT);
                            putClientProperty("Slider.variant", "volume");
                            putClientProperty("Slider.instantScrollEnabled", Boolean.TRUE);
                            putClientProperty("Slider.volume.showIcon", Boolean.TRUE);
                        }});
                    }});
                    panel.add(new JPanel() {{
                        setLayout(new BoxLayout(this, BoxLayout.X_AXIS));
                        add(new JSlider() {{
                            setOrientation(VERTICAL);
                        }});
                        add(new JSlider() {{
                            setOrientation(VERTICAL);
                            setInverted(true);
                        }});
                        add(new JSlider() {{
                            setOrientation(VERTICAL);
                            putClientProperty("Slider.variant", "volume");
                            putClientProperty("Slider.instantScrollEnabled", Boolean.TRUE);
                        }});
                        add(new JSlider() {{
                            setOrientation(VERTICAL);
                            putClientProperty("Slider.variant", "volume");
                            putClientProperty("Slider.instantScrollEnabled", Boolean.TRUE);
                            putClientProperty("Slider.volume.showIcon", Boolean.TRUE);
                        }});
                        add(new JSlider() {{
                            setOrientation(VERTICAL);
                            setComponentOrientation(ComponentOrientation.RIGHT_TO_LEFT);
                            putClientProperty("Slider.variant", "volume");
                            putClientProperty("Slider.instantScrollEnabled", Boolean.TRUE);
                            putClientProperty("Slider.volume.showIcon", Boolean.TRUE);
                        }});
                    }});
                    panel.add(new JPanel() {{
                        setLayout(new BoxLayout(this, BoxLayout.X_AXIS));
                        add(new JSlider() {{
                            setOrientation(VERTICAL);
                            setSnapToTicks(true);
                            setPaintTicks(true);
                            setMajorTickSpacing(20);
                            setMinorTickSpacing(5);
                            setPaintLabels(true);
                        }});
                        add(new JSlider() {{
                            setOrientation(VERTICAL);
                            setSnapToTicks(true);
                            setPaintTicks(true);
                            setMajorTickSpacing(20);
                            setMinorTickSpacing(5);
                            setPaintLabels(true);
                            setComponentOrientation(ComponentOrientation.RIGHT_TO_LEFT);
                        }});
                        add(new JSlider() {{
                            setOrientation(VERTICAL);
                            setSnapToTicks(true);
                            setPaintTicks(true);
                            setMajorTickSpacing(20);
                            setMinorTickSpacing(5);
                            setPaintLabels(true);
                            setInverted(true);
                        }});
                        add(new JSlider() {{
                            setOrientation(VERTICAL);
                            setSnapToTicks(true);
                            setPaintTicks(true);
                            setMajorTickSpacing(20);
                            setMinorTickSpacing(5);
                            setPaintLabels(true);
                            setInverted(true);
                            setComponentOrientation(ComponentOrientation.RIGHT_TO_LEFT);
                        }});
                    }});
                    panel.add(new JPanel() {{
                        setLayout(new BoxLayout(this, BoxLayout.X_AXIS));
                        add(new JSlider() {{
                            setOrientation(VERTICAL);
                            setSnapToTicks(true);
                            setPaintTicks(true);
                            setMajorTickSpacing(20);
                            setMinorTickSpacing(5);
                            setPaintLabels(true);
                            putClientProperty("Slider.variant", "volume");
                            putClientProperty("Slider.instantScrollEnabled", Boolean.TRUE);
                        }});
                        add(new JSlider() {{
                            setOrientation(VERTICAL);
                            setSnapToTicks(true);
                            setPaintTicks(true);
                            setMajorTickSpacing(20);
                            setMinorTickSpacing(5);
                            setPaintLabels(true);
                            putClientProperty("Slider.variant", "volume");
                            putClientProperty("Slider.instantScrollEnabled", Boolean.TRUE);
                            putClientProperty("Slider.volume.showIcon", Boolean.TRUE);
                        }});
                        add(new JSlider() {{
                            setOrientation(VERTICAL);
                            setSnapToTicks(true);
                            setPaintTicks(true);
                            setMajorTickSpacing(20);
                            setMinorTickSpacing(5);
                            setPaintLabels(true);
                            setComponentOrientation(ComponentOrientation.RIGHT_TO_LEFT);
                            putClientProperty("Slider.variant", "volume");
                            putClientProperty("Slider.instantScrollEnabled", Boolean.TRUE);
                            putClientProperty("Slider.volume.showIcon", Boolean.TRUE);
                        }});
                    }});

                    frame.setContentPane(content);

                    JMenuBar menuBar = new JMenuBar();
                    JMenu menu = new JMenu("test");
                    menu.add(new JMenu("submenu") {{
                        add(new JMenuItem("item1"));
                        add(new JMenuItem("item2"));
                        add(new JMenuItem("item3"));
                        add(new JMenuItem("item4"));
                        add(new JMenuItem("item5"));
                    }});
                    menu.addSeparator();
                    menu.add(new JRadioButtonMenuItem("radioButton"));
                    menu.add(new JCheckBoxMenuItem("checkBox"));
                    menuBar.add(menu);
                    frame.setJMenuBar(menuBar);

                    frame.getRootPane().setDefaultButton(defaultButton);
                    frame.setSize(1200, 800);
                    frame.setLocationRelativeTo(null);
                    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
                    frame.setVisible(true);
                });
    }
}
