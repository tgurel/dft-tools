/*
        *
        * FileInputDemo
        * Demonstrates the FileInputStream class
        * and DataInputStream
        */
import java.io.*;

class ExtractPhonons
{
        public static void main(String args[])
        {
                String outdata="";
                String outdatatemp="";
                String filename=args[1];
                // args.length is equivalent to argc in C
                if (args.length == 2)
                {
                        try
                        {
                                // Open the file that is the first 
                                // command line parameter
                                FileInputStream fstream = new 
                                        FileInputStream(args[0]);

                                // Convert our input stream to a
                                // DataInputStream
                                DataInputStream in = 
                                        new DataInputStream(fstream);

                                // Continue to read lines while 
                                // there are still some left to read
                                int counter=1;
                                int satirsayisi = 4;
                                while (in.available() !=0)
                                {
                                        //System.out.print(" "+counter+"--->");
                                        // Print file line to screen
                                        if ((counter%satirsayisi==0)){
                                                outdatatemp=in.readLine();
                                                outdatatemp=outdatatemp.replaceFirst("- ","");
                                                outdata = outdata + " " + outdatatemp+"\n";
                                                System.out.println (" "+outdatatemp);
                                        }
                                        else{
                                                outdatatemp=in.readLine();
                                                outdatatemp=outdatatemp.replaceFirst("- ","");
                                                outdata = outdata + outdatatemp;
                                                System.out.print (outdatatemp);

                                        }
                                        counter++;
                                }

                                in.close();
                        } 
                        catch (Exception e)
                        {
                                System.err.println("File input error");
                        }
                }
                else{
                        System.out.println("Invalid parameters");
        }


        FileOutputStream out; // declare a file output object
        PrintStream p; // declare a print stream object

        try{
                // Create a new file output stream
                // connected to "myfile.txt"
                out = new FileOutputStream(filename);

                // Connect print stream to the output stream
                p = new PrintStream( out );

                p.println (outdata);

                p.close();
        }
        catch (Exception e)
        {
                System.err.println ("Error writing to file");
        }

}
}

