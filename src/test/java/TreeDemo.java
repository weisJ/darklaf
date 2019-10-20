import com.github.weisj.darklaf.LafManager;
import com.github.weisj.darklaf.components.SelectableTreeNode;

import javax.swing.*;
import javax.swing.tree.DefaultMutableTreeNode;

public class TreeDemo extends JFrame {

    public TreeDemo() {
        super("JTree Demo");

        DefaultMutableTreeNode root = new DefaultMutableTreeNode("States");
        DefaultMutableTreeNode parent1 = new DefaultMutableTreeNode("Andhra Pradesh");
        DefaultMutableTreeNode child = new DefaultMutableTreeNode("Vijayawada");
        DefaultMutableTreeNode child1 = new SelectableTreeNode("This node can be selected", true);
        DefaultMutableTreeNode parent2 = new DefaultMutableTreeNode("Telangana");
        DefaultMutableTreeNode child2 = new DefaultMutableTreeNode("Hyderabad");

        // Adding child nodes to parent
        parent1.add(child);
        parent1.add(child1);
        parent2.add(child2);

        // Adding parent nodes to root
        root.add(parent1);
        root.add(parent2);

        // Adding root to JTree
        JTree tree = new JTree(root);
        tree.setEditable(true);
//        tree.setComponentOrientation(ComponentOrientation.RIGHT_TO_LEFT);

        getContentPane().add(new JScrollPane(tree));
        setSize(300, 300);
        setLocationRelativeTo(null);
        setVisible(true);
    }

    public static void main(final String[] args) {
        SwingUtilities.invokeLater(() -> {
            LafManager.install();
            new TreeDemo();
        });
    }
}
