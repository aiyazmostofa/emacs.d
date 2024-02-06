import java.util.*;
import java.io.*;
import java.math.*;

// %s
public class Solution {
    void run() {
    }

    CP.Reader in;
    PrintWriter out;

    public static void main(String[] args) throws Exception {
        CP.exec();
    }
}

class CP {
    static String IN = "%s";
    static String OUT = "%s";

    static class Reader {
        BufferedReader br;
        StringTokenizer st;

        Reader(InputStream is) {
            br = new BufferedReader(new InputStreamReader(is));
        }

        String next() {
            try {
                while (st == null || !st.hasMoreTokens())
                    st = new StringTokenizer(br.readLine());
                return st.nextToken();
            } catch (Exception ignored) {
            }
            return null;
        }

        int nextInt() {
            return Integer.parseInt(next());
        }

        long nextLong() {
            return Long.parseLong(next());
        }

        double nextDouble() {
            return Double.parseDouble(next());
        }
    }

    static void exec() throws Exception {
        Solution s = new Solution();
        s.in = new CP.Reader(IN.length() == 0 || System.getenv("CP_ENJOYER") != null ? System.in
                : new FileInputStream(IN));
        s.out = new PrintWriter(OUT.length() == 0 || System.getenv("CP_ENJOYER") != null ? System.out
                : new FileOutputStream(OUT));
        s.run();
        s.in.br.close();
        s.out.close();
    }
}
