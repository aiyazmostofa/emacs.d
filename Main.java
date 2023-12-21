import java.util.*;
import java.io.*;
import java.math.*;

// %s
public class Main {
    void run() {

    }

    static final String INPUT = "%s";
    static final String OUTPUT = "%s";
    final FastScanner in;
    final PrintWriter out;

    Main(InputStream input, OutputStream output) {
        in = new FastScanner(input);
        out = new PrintWriter(output);
    }

    void close() {
        out.close();
    }

    public static void main(String[] args) throws Exception {
        InputStream input = INPUT.length() == 0 || System.getenv("CP_ENJOYER") != null ? System.in
                : new FileInputStream(INPUT);
        OutputStream output = OUTPUT.length() == 0 || System.getenv("CP_ENJOYER") != null ? System.out
                : new FileOutputStream(OUTPUT);
        Main main = new Main(input, output);
        main.run();
        main.close();
    }

    class FastScanner {
        private BufferedReader bufferedReader;
        private StringTokenizer stringTokenizer;

        public FastScanner(InputStream inputStream) {
            bufferedReader = new BufferedReader(new InputStreamReader(inputStream));
        }

        public String next() {
            try {
                while (stringTokenizer == null || !stringTokenizer.hasMoreTokens())
                    stringTokenizer = new StringTokenizer(bufferedReader.readLine());
                return stringTokenizer.nextToken();
            } catch (Exception ignored) {
            }
            return null;
        }

        public int nextInt() {
            return Integer.parseInt(next());
        }

        public long nextLong() {
            return Long.parseLong(next());
        }

        public double nextDouble() {
            return Double.parseDouble(next());
        }
    }
}
