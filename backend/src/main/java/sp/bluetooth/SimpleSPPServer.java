package sp.bluetooth;

import javax.bluetooth.*;
import javax.microedition.io.*;
import java.io.IOException;

public class SimpleSPPServer {

    private BluetoothConnectionThread btConnectionThread;
    private StreamConnectionNotifier streamConnNotifier;
    private BluetoothMessageListener btMessageListener;

    public SimpleSPPServer(BluetoothMessageListener btMessageListener) {
        this.btMessageListener = btMessageListener;
    }

    public void startServer() throws IOException{

        //Create a UUID for SPP
        UUID uuid = new UUID("1101", true);
        String connectionString = "btspp://localhost:" + uuid +";name=Sample SPP Server";

        //open server url
        streamConnNotifier = (StreamConnectionNotifier) Connector.open( connectionString );

        //Wait for client connection
        System.out.println("\nServer Started. Waiting for clients to connect...");
        StreamConnection connection = streamConnNotifier.acceptAndOpen();

        RemoteDevice dev = RemoteDevice.getRemoteDevice(connection);
        System.out.println("Device Connected");
        System.out.println("Remote device address: "+dev.getBluetoothAddress());
        System.out.println("Remote device name: "+dev.getFriendlyName(true));

        btConnectionThread = new BluetoothConnectionThread(connection, btMessageListener);
        btConnectionThread.start();
    }

    public void sendMessage(String message) {
        btConnectionThread.write(message);
    }

    public void close() throws IOException {
        btConnectionThread.close();
        streamConnNotifier.close();
    }
}
