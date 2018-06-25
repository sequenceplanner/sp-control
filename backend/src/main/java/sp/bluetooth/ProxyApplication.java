package sp.bluetooth;

public class ProxyApplication implements BluetoothMessageListener {

    // Message counter for demonstrations purposes
    private int message_count;

    private BluetoothProxy proxy;
    public ProxyApplication() {
        // set message count to 0
        message_count = 0;

        // This builds the Bluetooth Proxy. The process will block until
        // a device connects to it. Might fail if multiple devices try to
        // connect at the same time.
        proxy = new BluetoothProxy(this);

        // Use proxy's send method to send messages to the device
        proxy.send("Hello! this is server");
    }

    // Use this method to redirect bluetooth messages through network
    @Override
    public void onBluetoothMessage(String message) {
        System.out.println("[RECEIVED] "+message);
        // Increase and notify message count
        message_count++;
        proxy.send("Received "+ message_count + (message_count == 1 ? " message" :  "messages"));
    }

    public static void main(String[] args) {
        new ProxyApplication();
    }
}
