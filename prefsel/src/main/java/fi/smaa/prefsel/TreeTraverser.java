package fi.smaa.prefsel;

import java.util.HashMap;
import java.util.Map;

@SuppressWarnings("rawtypes")
public class TreeTraverser {
	
	public static int nrNodes(Node root) {
		int count = 1;
		
		for (Node qn : root.getChildren()) {
			count += nrNodes(qn);
		}
		return count;
	}
	
	public static String toDOT(Node root) {
		String res = "graph G {\n";
		res += nodeToDOT(root, new Sequencer(), new HashMap<Node, Integer>());
		res += "}\n";
		return res;
	}
	
	private static String nodeToDOT(Node root, Sequencer sequencer, Map<Node, Integer> sequences) {
		String nodeS = nodeString(root, sequencer, sequences);
		
		String res = nodeS + "\t[label=\"" + nodeLabel(root) + "\"];\n";
		for (Node qn : root.getChildren()) {
			res += "\t" + nodeS + " -- " + nodeString(qn, sequencer, sequences) + ";\n";
		}
		for (Node qn : root.getChildren()) {
			res += nodeToDOT(qn, sequencer, sequences);
		}
		
		return res;
	}
	
	private static String nodeLabel(Node n) {
		if (n instanceof UnexpandedNode) {
			return "un";
		} else if (n instanceof ConcreteAnswerNode) {
			ConcreteAnswerNode a = (ConcreteAnswerNode) n;
			return Integer.toString(a.getAnswer());
		} else if (n instanceof QuestionNode){
			QuestionNode q = (QuestionNode) n;
			return q.getQuestion().getA1() + " ? " + q.getQuestion().getA2();
		} else {
			throw new IllegalArgumentException("strange node");
		}
	}

	private static String nodeString(Node root, Sequencer sequencer,
			Map<Node, Integer> sequences) {
		int seq = -1;
		if (sequences.containsKey(root)) {
			seq = sequences.get(root);
		} else {
			seq = sequencer.next();
			sequences.put(root, seq);
		}	
		String rootS = root.toString();
		if (rootS.equals("a-1")) {
			rootS = "R";
		}
	
		return rootS + "seq" + seq;
	}

	public static int getHeight(Node root) {
		if (root == null) {
			return 0;
		}
		int maxH = 0;
		for (Node n : root.getChildren()) {
			maxH = Math.max(maxH,  getHeight(n));
		}
		return maxH + 1;
	}

	public static int getMinNrQuestions(AnswerNode root) {
		if (root.getChildren().length < 1) {
			return 0;
		}
		int minQ = Integer.MAX_VALUE;
		for (QuestionNode qn : root.getChildren()) {
			for (AnswerNode an : qn.getChildren()) {
				minQ = Math.min(minQ, getMinNrQuestions(an) + 1);
			}
		}
		return minQ;
	}

	public static int getMaxNrQuestions(AnswerNode root) {
		if (root.getChildren().length < 1) {
			return 0;
		}
		int maxQ = Integer.MIN_VALUE;
		for (QuestionNode qn : root.getChildren()) {
			for (AnswerNode an : qn.getChildren()) {
				maxQ = Math.max(maxQ, getMaxNrQuestions(an) + 1);
			}
		}
		return maxQ;
	}
	
}
