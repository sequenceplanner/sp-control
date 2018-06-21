package sp.bluetooth;

import java.io.IOException;
import javax.bluetooth.*;

public class BluetoothProxy {

    private LocalDevice localDevice;
    private SimpleSPPServer server;

    public BluetoothProxy(BluetoothMessageListener btMessageListener) {


        try {
            localDevice = LocalDevice.getLocalDevice();
        } catch (BluetoothStateException e) {
            System.out.println("Could not find local bluetooth device.");
            localDevice = null;
            e.printStackTrace();
        }

        if (localDevice != null) startServer(btMessageListener);
    }

    public void send(String message) {
        server.sendMessage(message);
    }

    private void startServer(BluetoothMessageListener btMessageListener){

        System.out.println("Starting bluetooth proxy server");
        System.out.println("Address: "+localDevice.getBluetoothAddress());
        System.out.println("Name: "+localDevice.getFriendlyName());

        server = new SimpleSPPServer(btMessageListener);
        try {
            server.startServer();
        } catch (IOException e) {
            System.out.println("Error ocurred while starting the server");
            e.printStackTrace();
        }

    }
}
