package sp.bluetooth;

import javax.microedition.io.StreamConnection;
import java.io.*;

public class BluetoothConnectionThread extends Thread{

    private StreamConnection connection;
    private InputStream inputStream;
    private OutputStream outputStream;
    private PrintWriter printWriter;
    private BufferedReader bufferedReader;
    private BluetoothMessageListener bluetoothClient;

    public BluetoothConnectionThread(StreamConnection connection, BluetoothMessageListener bluetoothClient) {
        this.connection = connection;
        this.bluetoothClient = bluetoothClient;

        try {
            inputStream = this.connection.openInputStream();
            bufferedReader = new BufferedReader(new InputStreamReader(inputStream));

            outputStream = this.connection.openOutputStream();
            printWriter = new PrintWriter(new OutputStreamWriter(outputStream));
        } catch (IOException e) {
            System.out.println("Error opening input/output streams");
            e.printStackTrace();
        }
    }

    public boolean streamsReady() {
        return outputStream != null && inputStream != null;
    }

    @Override
    public void run() {

        while (streamsReady()) {
            try {
                String message = bufferedReader.readLine();

                if (message == null) break;

                bluetoothClient.onBluetoothMessage(message);
            } catch (IOException e) {
                System.out.println("Error while reading from client");
                e.printStackTrace();
            }
        }

    }

    public void write(String message) {
        printWriter.write(message + "\r\n");
        printWriter.flush();
    }

    public void close() {
        printWriter.close();
    }
}
