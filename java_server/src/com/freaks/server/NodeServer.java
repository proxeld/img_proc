package com.freaks.server;

import com.ericsson.otp.erlang.*;
import com.freaks.Settings;

import javax.imageio.ImageIO;
import javax.swing.*;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.Date;
import java.util.concurrent.TimeoutException;
import java.util.logging.Logger;

/**
 * Created by proxeld on 10.01.15.
 */
public class NodeServer {
    private OtpNode self;
    private OtpMbox mbox;
    private OtpErlangTuple msg;
    private OtpErlangPid from;
    private OtpErlangBinary image;
    private OtpErlangString operation;
    private OtpConnection connection;
    private String peer = Settings.OTP_NODE_NAME;
    private String cookie = Settings.OTP_NODE_COOKIE;
    private String clientName = "javaServer";
    private Logger logger = Logger.getLogger("javaServer");


    /**
     * Start java server node and connect to erlang node
     * @param name name of the server
     */
    public NodeServer(String name) {

        try {
            logger.info("Starting java server node: name(" + Settings.JAVA_SERVER_NODE_NAME + ")," +
                    " mboxname(" + Settings.JAVA_SERVER_MAILBOX_NAME + "), cookie(" + Settings.JAVA_SERVER_COOKIE + ")");
            self = new OtpNode(Settings.JAVA_SERVER_NODE_NAME, Settings.JAVA_SERVER_COOKIE);
            mbox = self.createMbox(Settings.JAVA_SERVER_MAILBOX_NAME);

            connect();

        } catch (Exception e) {
            logger.warning("Erlang " + Settings.OTP_NODE_NAME + " is unreachable. Check Settings and weather erlang node is running.");
            disconnect();
        }

        File tmpFolder = new File(Settings.TMP_FOLDER_NAME);
        tmpFolder.mkdir();
    }

    /**
     * Connects with Erlang Node or throws exception
     * @throws java.io.IOException
     * @throws OtpAuthException
     */
    public void connect() throws IOException, OtpAuthException, TimeoutException {
        logger.info("Please wait, connecting to " + peer + " with cookie " + cookie + "...");

        OtpNode pingNode = new OtpNode("ping", cookie);
        OtpPeer other = new OtpPeer(peer);
        OtpSelf self = new OtpSelf(clientName, cookie);

        logger.info("Pinging remote node...");
        if(!pingNode.ping(peer, Settings.TIMEOUT_THRESHOLD))
            throw new TimeoutException("Cannot ping erlang node");
        pingNode.close();
        logger.info("Remote host responded!");

        connection = self.connect(other);

        logger.info("Connection Established with " + peer);
    }

    /**
     * Closes opened connection
     */
    public void disconnect() {
        logger.info("Disconnecting...");

        if(connection != null)
            connection.close();

        connection = null;

        logger.info("Successfully disconnected.");
    }

    /**
     * Executes Remote Procedure Call and waits for response
     * @param module module name
     * @param function
     * @param argList
     * @return
     * @throws java.io.IOException
     * @throws OtpAuthException
     * @throws OtpErlangExit
     */
    public OtpErlangObject doRPC(String module, String function, OtpErlangList argList) throws IOException, OtpAuthException, OtpErlangExit {

        connection.sendRPC(module, function, argList);
        return connection.receiveRPC();
    }

    /**
     * Checks if connection exists
     * @return true if connection is established, false otherwise
     */
    public boolean isConnected() {
        return connection != null && connection.isConnected();
    }


    /**
     * Listen for requests
     */
    public void listen() {

        while(true) {
            if(!isConnected()) {
                logger.info("Erlang node not detected. Terminating.");
                return;
            }

            // MESSAGE STRUCTURE
            // OtpErlangTuple(sender: OtpErlangPid, image: OtpErlangBinary, operation: OtpErlangString)

            try {
                logger.info("Waiting for message...");
                OtpErlangObject msgFromClient = mbox.receive();
                logger.info("Message received: " + msgFromClient.toString());

                if (msgFromClient instanceof OtpErlangTuple) {
                    msg = (OtpErlangTuple) msgFromClient;
                    from = (OtpErlangPid) (msg.elementAt(0));
                    OtpErlangTuple args = (OtpErlangTuple) (msg.elementAt(1));
                    image = (OtpErlangBinary) (args.elementAt(0));
                    operation = (OtpErlangString) (args.elementAt(1));

                    OtpErlangObject response = processRequest(from.toString());

                    mbox.send(from, response);
                }

                if (msgFromClient instanceof OtpErlangTuple) {
                    msg = (OtpErlangTuple) msgFromClient;
                    from = (OtpErlangPid) (msg.elementAt(0));
                    mbox.send(from, msg.elementAt(1));
                }
            } catch(IOException e){
                e.printStackTrace();
            } catch (OtpErlangDecodeException e) {
                e.printStackTrace();
            } catch (OtpErlangExit otpErlangExit) {
                otpErlangExit.printStackTrace();
            } catch (OtpAuthException e) {
                e.printStackTrace();
            }
        }
    }


    /**
     * Process request
     * @param salt used for generating file name - should differ depending on client
     * @return processed image
     * @throws java.io.IOException
     * @throws OtpErlangExit
     * @throws OtpAuthException
     */
    private OtpErlangObject processRequest(String salt) throws IOException, OtpErlangExit, OtpAuthException {

        ImageIcon icon = (ImageIcon)(image.getObject());

        BufferedImage bi = imageIconToBufferdImage(icon);

        String tmpInImageName = NodeServer.generateFileName(salt + "_source");
        String tmpOutputImageName = NodeServer.generateFileName(salt + "_out");
        File source = new File(tmpInImageName);
        File output = new File(tmpOutputImageName);
        ImageIO.write(bi, "png", source);

        OtpErlangTuple erlImage = (OtpErlangTuple) doRPC(Settings.IMG_PROCESSING_MODULE_NAME, "load", new OtpErlangList(new OtpErlangString(source.getAbsolutePath())));
        OtpErlangObject erlImageDone = doRPC(Settings.IMG_PROCESSING_MODULE_NAME, operation.stringValue(), new OtpErlangList(erlImage.elementAt(1)));
        OtpErlangObject finalResponse = doRPC(Settings.IMG_PROCESSING_MODULE_NAME, "save", new OtpErlangList(new OtpErlangObject[] {erlImageDone, new OtpErlangString(output.getAbsolutePath())}));

        logger.info(finalResponse.toString());

        BufferedImage processedImage = ImageIO.read(output);
        OtpErlangObject response = new OtpErlangBinary(new ImageIcon(processedImage));

        return response;
    }

    /**
     * Generates filename
     * @param salt
     * @return
     */
    private static String generateFileName(String salt) {
        Date currentDate = new Date();
        return Settings.TMP_FOLDER_NAME + "/" + currentDate.getTime() + salt + ".png";
    }

    /**
     * Helper function. Converts ImageIcon to BufferedImage
     * @param icon
     * @return
     */
    private  BufferedImage imageIconToBufferdImage(ImageIcon icon) {
        BufferedImage bi = new BufferedImage(
                            icon.getIconWidth(),
                            icon.getIconHeight(),
                            BufferedImage.TYPE_BYTE_GRAY);
        Graphics g = bi.createGraphics();
        icon.paintIcon(null, g, 0,0);
        g.dispose();

        return bi;
    }
}
