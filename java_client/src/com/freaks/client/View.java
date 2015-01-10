package com.freaks.client;

import javax.imageio.ImageIO;
import javax.swing.*;
import javax.swing.filechooser.FileFilter;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.logging.Logger;

/**
 * Created by proxeld on 09.01.15.
 * Application view
 * @author proxeld
 */
public class View extends JFrame {
    private Controller controller;
    private BufferedImage sourceImage;
    private BufferedImage processedImage;
    private Logger guiLoger = Logger.getLogger("guiLogger");

    private JPanel rootPanel;
    private JButton maxButton;
    private JButton averageButton;
    private JButton minButton;
    private JButton gaussButton;
    private JPanel sourceImagePanel;
    private JPanel resultImagePanel;
    private JButton connectButton;
    private JButton medianButton;

    private JMenuBar menuBar;
    private JMenu fileMenu;
    private JMenu helpMenu;
    private JMenuItem openFileMenuItem;
    private JMenuItem saveFileMenuItem;
    private JMenuItem aboutMenuItem;
    private JFileChooser fileChooser;


    /**
     * Main view creation
     */
    public View(Controller _controller) {
        super("Image processing");

        setContentPane(rootPanel);

        customCreation();
        bindEvents();

        pack();
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

        controller = _controller;
        controller.setView(this);
        setVisible(true);
    }

    private void customCreation() {

        // fileMenu bar creation
        menuBar = new JMenuBar();
        fileMenu = new JMenu("File");
        fileMenu.setMnemonic(KeyEvent.VK_F);

        openFileMenuItem = new JMenuItem("Load image...", KeyEvent.VK_L);
        fileMenu.add(openFileMenuItem);

        saveFileMenuItem = new JMenuItem("Save image", new ImageIcon("images/save.gif"));
        saveFileMenuItem.setMnemonic(KeyEvent.VK_S);
        fileMenu.add(saveFileMenuItem);

        helpMenu = new JMenu("Help");
        helpMenu.setMnemonic(KeyEvent.VK_H);

        aboutMenuItem = new JMenuItem("About", KeyEvent.VK_A);
        helpMenu.add(aboutMenuItem);

        menuBar.add(fileMenu);
        menuBar.add(helpMenu);
        setJMenuBar(menuBar);

        // file chooser
        fileChooser = new JFileChooser();

        FileFilter ff = new FileFilter() {
            @Override
            public boolean accept(File file) {
                if (file.isDirectory()) {
                    return true;
                }

                String trimmed = file.getName().trim();
                return trimmed.substring(trimmed.lastIndexOf(".")+1).toLowerCase().equals("png");
            }

            @Override
            public String getDescription() {
                return "PNG Images";
            }
        };

        fileChooser.setAcceptAllFileFilterUsed(false);
        fileChooser.setFileFilter(ff);
    }

    private void bindEvents() {

        openFileMenuItem.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent actionEvent) {

                int result = fileChooser.showDialog(View.this, "Choose image");

                if (result == JFileChooser.APPROVE_OPTION) {
                    File image = fileChooser.getSelectedFile();

                    try {
                        BufferedImage myPicture = ImageIO.read(image);
                        JLabel picLabel = new JLabel(new ImageIcon(myPicture));
                        sourceImage = myPicture;
                        sourceImagePanel.removeAll();
                        sourceImagePanel.add(picLabel);
                        rootPanel.repaint();
                        rootPanel.invalidate();

                    } catch(IOException e) {
                        e.printStackTrace();
                    }

                    guiLoger.info("Opening file: " + image.getName());
                }
            }
        });

        saveFileMenuItem.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent actionEvent) {
                int result = fileChooser.showSaveDialog(View.this);

                if (result == JFileChooser.APPROVE_OPTION) {

                    File imageFile = fileChooser.getSelectedFile();
                    guiLoger.info("Saving file: " + imageFile.getName());
                    controller.onSave(processedImage, imageFile);
                }
            }
        });

        aboutMenuItem.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent actionEvent) {
                showInfoPopup("Authors: Maciej Urbanek & Jakub Pelczar\nVersion: 0.0.2");
            }
        });

        connectButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent actionEvent) {
                controller.onConnect();
            }
        });

        averageButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent actionEvent) {
            controller.onOperationChosen(sourceImage, "filterAverage");
            }
        });

        gaussButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent actionEvent) {
                controller.onOperationChosen(sourceImage, "filterGauss");
            }
        });

        medianButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent actionEvent) {
                controller.onOperationChosen(sourceImage, "filterMedian");
            }
        });

        minButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent actionEvent) {
                controller.onOperationChosen(sourceImage, "filterMin");
            }
        });

        maxButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent actionEvent) {
                controller.onOperationChosen(sourceImage, "filterMax");
            }
        });
    }

    private void createUIComponents() {
        sourceImagePanel = new JPanel();
        resultImagePanel = new JPanel();
    }

    void setProcessedImage(ImageIcon imageIcon) {

        BufferedImage bi = new BufferedImage(
                imageIcon.getIconWidth(),
                imageIcon.getIconHeight(),
                BufferedImage.TYPE_BYTE_GRAY);
        Graphics g = bi.createGraphics();
        imageIcon.paintIcon(null, g, 0,0);
        g.dispose();

        processedImage = bi;
    }

    void displayProccessedImage(ImageIcon imageIcon) {
        JLabel picLabel = new JLabel(imageIcon);
        resultImagePanel.removeAll();
        resultImagePanel.add(picLabel);
        rootPanel.invalidate();
        rootPanel.repaint();
    }

    void showInfoPopup(String msg) {
        JOptionPane.showMessageDialog(View.this, msg);
    }

    void showWarningPopup(String msg) {
        JOptionPane.showMessageDialog(View.this, msg, "Warning", JOptionPane.WARNING_MESSAGE);
    }

    void showErrorPopup(String msg) {
        JOptionPane.showMessageDialog(View.this, msg, "Something went wrong...", JOptionPane.ERROR_MESSAGE);
    }

    public void setController(Controller _controller) {
        controller = _controller;
    }
}
