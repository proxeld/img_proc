package com.freaks.client;

import com.ericsson.otp.erlang.*;
import com.freaks.Settings;

import javax.swing.*;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.util.Set;
import java.util.concurrent.TimeoutException;
import java.util.logging.Logger;

/**
 * Client for connecting with Erlang Node
 */
public class NodeClient {

    private OtpConnection connection;
    private OtpMbox mbox;
    private String peer;
    private String cookie;
    private String clientName;
    private Logger mainLogger = Logger.getLogger("nodeClientLogger");

    public NodeClient(String _peer, String _cookie, String _clientName) {
        peer = _peer;
        cookie = _cookie;
        clientName = _clientName;
    }

    /**
     * Connects with Erlang Node or throws exception
     * @throws IOException
     * @throws OtpAuthException
     */
    public void connect() throws IOException, OtpAuthException, TimeoutException {
        mainLogger.info("Please wait, connecting to " + peer + " with cookie " + cookie + "...");

        OtpNode pingNode = new OtpNode("ping", cookie);
        OtpPeer other = new OtpPeer(peer);
        OtpSelf self = new OtpSelf(clientName, cookie);

        mainLogger.info("Pinging remote node...");
        if(!pingNode.ping(peer, Settings.TIMEOUT_THRESHOLD))
            throw new TimeoutException("Cannot ping erlang node");
        pingNode.close();
        mainLogger.info("Remote host responded!");

        connection = self.connect(other);

        mainLogger.info("Connection Established with " + peer);
    }

    /**
     * Closes opened connection
     */
    public void disconnect() {
        mainLogger.info("Disconnecting...");

        if(connection != null)
            connection.close();

        connection = null;

        mainLogger.info("Successfully disconnected.");
    }

    /**
     * Checks if connection exists
     * @return true if connection is established, false otherwise
     */
    public boolean isConnected() {
        return connection != null && connection.isConnected();
    }


    public void sendMessage(OtpErlangTuple args) throws IOException {

        // MESSAGE STRUCTURE
        // OtpErlangTuple(sender: OtpErlangPid, OtpErlangTuple(image: OtpErlangBinary, operation: OtpErlangString))

        OtpErlangObject[] msg = new OtpErlangObject[] {
                connection.self().pid(),
                args
        };

        OtpErlangTuple tuple = new OtpErlangTuple(msg);
        connection.send(Settings.JAVA_SERVER_MAILBOX_NAME, tuple);
    }

    public OtpErlangObject receiveResponse() throws InterruptedException, OtpErlangExit, OtpAuthException, IOException {
        return connection.receive(Settings.TIMEOUT_THRESHOLD);
    }

}