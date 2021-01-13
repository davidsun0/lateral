package lateral.interactive;

import javax.swing.*;
import java.awt.*;

public class Canvas extends JPanel {

    @Override
    protected void paintComponent(Graphics g) {
        super.paintComponent(g);
        // call lisp here with graphics
        g.setColor(Color.BLACK);
        g.fillRect(100, 100, 50, 50);
    }

    public static void main(String[] args) {
        // load file here
        SwingUtilities.invokeLater(() -> {
            Canvas canvas = new Canvas();
            canvas.setBackground(Color.WHITE);
            JFrame frame = new JFrame("Lateral Canvas");
            frame.setSize(500, 500);
            frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
            frame.getContentPane().add(canvas, BorderLayout.CENTER);
            frame.setVisible(true);
        });
    }
}
