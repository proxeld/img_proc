package com.freaks.server;

import com.freaks.Settings;

public class ServerStart {

    public static void main(String[] argv) {
        NodeServer server = new NodeServer(Settings.JAVA_SERVER_NODE_NAME);
        server.listen();
    }
}
