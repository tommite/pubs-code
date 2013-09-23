package fi.smaa.prefsel;


public class TreeTraverser {
	
	public static int nrNodes(AnswerNode root) {
		int count = 1;
		
		for (QuestionNode qn : root.getChildren()) {
			count += 1;
			if (qn.getLeftChild() != null) {
				count += nrNodes(qn.getLeftChild());
			}
			if (qn.getRightChild() != null) {
				count += nrNodes(qn.getRightChild());
			}			
		}
		return count;
	}
	
	public static String toDOT(AnswerNode root) {
		String res = "graph G {\n";
		res += answerNodeToDOT(root);
		res += "}\n";
		return res;
	}

	private static String answerNodeToDOT(AnswerNode root) {
		int ans = root.getAnswer();
		
		String res = "";
		
		for (QuestionNode qn : root.getChildren()) {
			Question q = qn.getQuestion();
			res += "\t" + ans + " -- " + q.dotString() + "\n";
		}
		for (QuestionNode qn : root.getChildren()) {
			res += questionNodeToDOT(qn);
		}
		return res;
	}

	private static String questionNodeToDOT(QuestionNode root) {
		String res = "";
		if (root.getLeftChild() != null) {
			res += answerNodeToDOT(root.getLeftChild());
		}
		if (root.getRightChild() != null) {
			res += answerNodeToDOT(root.getLeftChild());
		}		
		return res;
	}

}
