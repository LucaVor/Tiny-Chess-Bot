using ChessChallenge.API;
using System.Linq;

public class MyBot : IChessBot
{
    Timer timerReference;
    float millisecondLimit;

    // Returns if we should abort search
    bool ShouldAbort => timerReference.MillisecondsElapsedThisTurn > millisecondLimit;

    // Total number of positions searched in tree
    int numPositionsSearched = 0;

    public Move Think(Board board, Timer timer)
    {
        // Store in variable for less tokens per reference
        millisecondLimit = timer.MillisecondsRemaining;
        // Calculate the number of milliseconds to allocate
        // by the stage of the game
        millisecondLimit = millisecondLimit > 5000 ? 2000 - (millisecondLimit*millisecondLimit*millisecondLimit) / 144000000000 : millisecondLimit / 10;

        // Empty out table
        table = new Transposition[1048576];

        // Stores best move found so far - IN SEARCH
        bestMove = Move.NullMove;
        timerReference = timer;

        // Best move found so far
        Move bestReturnedMove = Move.NullMove;

        for (int depth = 1; depth < 0xFFFFF; depth++)
        {
            numPositionsSearched = 0;

            // Conduct search
            Search(board, float.MinValue, float.MaxValue, depth, 0, 0);

            // If we are finished
            if (ShouldAbort)
            {
                // If we have looked at at-least one position
                // return best move
                if (numPositionsSearched > 0)
                    return bestMove;
            }

            // Set best move so far as currently searched move
            bestReturnedMove = bestMove;
        }

        return bestReturnedMove;
    }

    // stores data about a transposition
    struct Transposition { public ulong MyKey; public float Value; public int depth, bound; public Move Move; }

    // initializes table
    Transposition[] table = new Transposition[1048576];

    // List of piece square values, encoded into bytes

    /* Values in the piece square tables are encoded into these numbers
     * with each piecevalue being a 8bit integer, going to 255
     * 
     * Because some of our values are > 255, they are half of their actual values
     * and then I double it here. That leads to the loss of some data
     * however it is +- 2.
     */

    static float[][] pieceSquareTables = new ulong[]
    {
        6004234345560363859, 5648800866043598468, 5287348508207109968,
        5214140734727674444, 5140953929278902854, 5576676063466049862,
        5216961999590216514, 6004234345560363859, 2183158892895282944,
        5428933629868195631, 7599987637732405564, 6799439719732960335,
        5719111249516254541, 5431442706592780104, 5353755569969510725,
        5209052108559894815, 5717106727727027525, 4349476007150508870,
        5937552341517035083, 5933323624174671441, 6149763578822548048,
        6367071013902703443, 6081089126010674005, 5278303757615780419,
        7594010649456502883, 7593198144808051553, 6589446149866806609,
        5282552348241514055, 5212441959012845121, 4850749899455285053,
        3481380726304754493, 5062423506043817290, 7667775776126817093,
        7953761885752475719, 8100690822918785869, 6076020360091485766,
        5932737498073482831, 6222378543415710796, 6076019277911641922,
        4198559145840822867, 6508900166728900403, 4990077788217758562,
        5214700291338035023, 4705214069258144075, 4198266606625116987,
        5065498710680030284, 6293863209771161428, 6511999827110880588,
        6004234345560363859, 12801083494916860588, 9042223576844764546,
        6655288161471192931, 6004792884531976282, 5716001770401978197,
        5788343068572211034, 6004234345560363859, 2464672155112914998,
        4127345989516742471, 4560544792049109319, 5356848543625532747,
        5356288879355449418, 5208784983473476168, 4415860052537002302,
        3691344518961445445, 5137287006292691276, 5499261635876311375,
        6148352827876134740, 6076860417258903634, 5715728009338180944,
        5498988991536910925, 5065510908773550668, 5427201829996351304,
        6221539650555369562, 6149477628372474457, 5931612710697916247,
        6076293043422123349, 5642817168372880981, 5426641083395298129,
        5930764961429278800, 5284214749474935887, 6726228716304621135,
        6008470888769609035, 6367356960225384009, 7309455927740948053,
        6874573734370173258, 6221824419688040011, 4846234162249877576,
        4560250113905673539, 3113464301390607659, 3836866029771374389,
        4560267758152141119, 5283669486532907849, 5283669486532907849,
        4560267758152141119, 3836866029771374389, 3113464301390607659
    }
        // Convert to rows of eight
        .Select(a => new float[8].Select((b, i) => (float)(((a >> i * 8) & 0b11111111) * 2) - 166))
        // Flatten
        .SelectMany(a => a)
        // Chunk into 8x8 arrays
        .Chunk(64)
        // Apply piece values
        .Select((a, i) => a.Select(b => b + new float[] { 82, 337, 365, 477, 1025, 0, 94, 281, 297, 512, 936, 0 }[i]).ToArray())
        .ToArray();

    // Evaluate board position
    float Evaluate(Board board, float movePerspective)
    {
        // If checkmate, return BAD
        if (board.IsInCheckmate())
            return -0xFFFFF;

        // If draw, return ambivalence
        if (board.IsDraw())
            return 0;

        // Current phase in the game
        // used to interpolate between mid and end game values
        int gamePhase = 0;


        // Extract game values based on board state and colour
        (float wMid, float wEnd) = PESTO(board, true, ref gamePhase);
        (float bMid, float bEnd) = PESTO(board, false, ref gamePhase);

        return ((wMid - bMid) * gamePhase + (wEnd - bEnd) * (24 - gamePhase)) * movePerspective / 24;
    }

    // Amount to increment based on pieceIndex
    int[] gamePhaseIncrement = new int[] { 0, 1, 1, 2, 4, 0 };

    (float, float) PESTO(Board board, bool isWhite, ref int gamePhase)
    {
        // Store empty values
        float mid = 0, end = 0;

        // For each piece, Pawn -> King
        for (int pieceType = 1; pieceType <= 6; pieceType++)
        {
            // Get list of pieces
            PieceList pieceList = board.GetPieceList((PieceType)pieceType, isWhite);

            // Inc game phase
            gamePhase += gamePhaseIncrement[pieceType - 1] * pieceList.Count;

            // Foreach piece
            for (int index = 0; index < pieceList.Count; index++)
            {
                // Piece square
                int square = pieceList.GetPiece(index).Square.Index;

                // Invert if it is white
                if (isWhite)
                    square = 63 - square;

                // Reference table
                mid += pieceSquareTables[pieceType - 1][square];
                end += pieceSquareTables[pieceType + 5][square];
            }
        }

        return (mid, end);
    }

    Move bestMove;

    // Quiet search, searches all captures
    float Quiesce(Board node, float alpha, float beta)
    {
        // Abort if shoud abort
        if (ShouldAbort)
            return 0;

        // Evaluate position
        float evaluation = Evaluate(node, node.IsWhiteToMove ? 1 : -1);

        // Beta cutoff
        if (evaluation >= beta)
            return beta;

        // Max alpha
        if (evaluation > alpha)
            alpha = evaluation;

        // Get all quiet moves, order by the piece that is being captured
        Move[] loudMoves = node.GetLegalMoves(true).OrderByDescending(a => (int)a.CapturePieceType).ToArray();

        // Foreach new move
        foreach (Move move in loudMoves)
        {
            // Make move on board
            node.MakeMove(move);

            // Collect score from subtree
            float score = -Quiesce(node, -beta, -alpha);

            // undo
            node.UndoMove(move);

            if (score >= beta)
                return beta;
            if (score > alpha)
                alpha = score;
        }

        return alpha;
    }

    public float Search(Board node, float alpha, float beta, int depth, int ply, int numExtensions)
    {
        // ABORT
        if (ShouldAbort)
            return 0;

        // Get index in transposition table
        ulong entryIndex = node.ZobristKey % 1048576;

        // Get entry
        Transposition entry = table[entryIndex];

        // If transposition matches position, and we searched it to the same, or greater, depth.
        if (entry.MyKey == node.ZobristKey && entry.depth >= depth)
        {
            // Get value
            float transpositionEval = entry.Value;

            // Apply bounds
            if (entry.bound == 0
                || (entry.bound == 1 && transpositionEval <= alpha)
                || (entry.bound == 2 && transpositionEval >= beta))
            {
                // Return transposition value

                if (ply == 0)
                    bestMove = entry.Move;

                return transpositionEval;
            }
        }

        if (depth == 0)
            return Quiesce(node, alpha, beta);

        // Get the primary move to look at first
        Move primaryMove = ply == 0 ? bestMove : entry.Move,
            localBestMove = Move.NullMove;
        
        // Order legal moves by capture piece type
        Move[] legalMoves = node.GetLegalMoves().OrderByDescending(a => (int)a.CapturePieceType
            + (int)a.PromotionPieceType
            + (a.RawValue == primaryMove.RawValue ? 0xFFFFF : 0)).ToArray();

        // Return checkmate/draw
        if (legalMoves.Length == 0)
            return node.IsDraw() ? 0 : -0xFFFFF;

        int evalBound = 1;

        // Foreach move
        foreach (Move move in legalMoves)
        {
            // Apply move in node
            node.MakeMove(move);

            // Extend checks
            int nodalExtension = node.IsInCheck() ? 1 : 0;

            // Cap the num of extensions at 3
            if (numExtensions > 3)
                nodalExtension = 0;

            // Collect score
            float score = -Search(node, -beta, -alpha, depth - 1 + nodalExtension, ply + 1, numExtensions + nodalExtension);

            // Undo move
            node.UndoMove(move);

            // ABORT
            if (ShouldAbort)
                return 0;

            // Cutoff beta
            if (score >= beta)
            {
                Store(entryIndex, node.ZobristKey, beta, depth, 2, move);

                return beta;
            }

            // If new best move
            if (score > alpha)
            {
                alpha = score;
                evalBound = 0;

                // Set move as best and modify alpha
                localBestMove = move;

                if (ply == 0)
                {
                    bestMove = move;

                    numPositionsSearched++;
                }
            }
        }

        Store(entryIndex, node.ZobristKey, alpha, depth, evalBound, localBestMove);

        return alpha;
    }

    // Store values in table
    void Store(ulong entryIndex, ulong zobrist, float alpha, int depth, int evalBound, Move localBest)
    {
        table[entryIndex].MyKey = zobrist;
        table[entryIndex].Value = alpha;
        table[entryIndex].depth = depth;
        table[entryIndex].bound = evalBound;
        table[entryIndex].Move = localBest;
    }
}
