package ParserCombinator;

/**
 * Class to wrap some string behavior and allow for much easier manipulation of input strings
 * by always holding onto the entire input string and making only a partial segment of it visible.
 *
 * @author Max Kopinsky
 */
class Stream {
    /** The string underlying this stream. */
    private String string;
    /** This stream's start position in the underlying stream. */
    private int cursor;
    /** The number of characters of the underlying stream which are visible. */
    private int length;

    /**
     * Full constructor. Only ever called inside this class when the stream is moved, to avoid reference interference.
     * @param s The string this stream should store.
     * @param c The position of this stream's cursor in the string.
     * @param l The length of this stream.
     */
    private Stream(String s, int c, int l) {
        string = s;
        cursor = c;
        length = l;
    }
    /**
     * Visible constructor. Creates a brand new stream from the input string.
     * @param s The string this stream should store.
     */
    Stream(String s) {
        string = s;
        cursor = 0;
        length = s.length();
    }

    /**
     * Getter for the length of the current stream.
     * @return length of this Stream's visible segment.
     */
    int length() {
        return length;
    }

    /**
     * toString() method provided for printing. To be used for debugging.
     * @return This stream's <em>visible segment</em>.
     */
    @Override
    public String toString() {
        if (length < 0) {throw new IllegalStateException("Stream length is negative."); }
        return string.substring(cursor, cursor + length);
    }

    /**
     * Gets the first character of this Stream's visible segment.
     * @return The first character of this Stream.
     */
    String head() {
        if (length <= 0) { throw new IllegalStateException("Stream is empty."); }
        return ((Character) string.charAt(cursor)).toString();
    }

    /**
     * Gets a new Stream (to avoid reference interference) with the cursor moved.
     * Negative distances are technically allowed, but should be avoided in favor of using a reference
     * to Stream in that position from before it was shifted to the current position.
     *
     * @param distance Distance to move the cursor.
     * @return Reference to a moved Stream.
     */
    Stream move(int distance) {
        return new Stream(string, cursor + distance, length - distance);
    }

    /**
     * Convenience method. Equivalent to the String class .substring(start, stop) method. Currently unused.
     * @param start Starting index (inclusive) of the slice.
     * @param stop Ending index (exclusive) of the slice.
     * @return Reference to a sliced Stream.
     */
    Stream slice(int start, int stop) {
        if (stop < start) { throw new IllegalArgumentException("stop < start"); }
        if (start < 0 || stop > length) { throw new IllegalArgumentException("Index out of range"); }
        return new Stream(string, cursor + start, stop - start);
    }

    /**
     * Convenience method. Equivalent to the String class .substring(start) method. Currently unused.
     * @param start Starting index (inclusive) of the slice.
     * @return Reference to a sliced Stream.
     */
    Stream slice(int start) {
        if (start < 0) {throw new IllegalArgumentException("Index out of range"); }
        return new Stream(string, cursor + start, length - start);
    }
}
