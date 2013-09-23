package fi.smaa.prefsel;

import java.util.ArrayList;
import java.util.Collections;

public class AnswerNode implements Node {

	private int answer;
	private QuestionNode[] children;
	private TransitiveRelation currentPreferences;

	public static final int NO_ANSWER = -1;

	public AnswerNode(Question[] remainingQuestions, int answer, TransitiveRelation prefs) {
		this.answer = answer;
		this.currentPreferences = prefs;
		createChildren(remainingQuestions);
	}

	private void createChildren(Question[] remainingQuestions) {
		children = new QuestionNode[remainingQuestions.length];
		for (int i=0;i<remainingQuestions.length;i++) {
			children[i] = new QuestionNode(remainingQuestions[i], cloneArrayWithoutOne(remainingQuestions, i), currentPreferences);
		}		
	}

	private Question[] cloneArrayWithoutOne(Question[] remainingQuestions, int i) {
		ArrayList<Question> qs = new ArrayList<Question>();
		Collections.addAll(qs, remainingQuestions);
		qs.remove(i);
		return qs.toArray(new Question[0]);
	}

	/**
	 * Build a root node (no answer)
	 * 
	 * @param questions
	 */
	public AnswerNode(Question[] questions, int nrAlts) {
		answer = NO_ANSWER;
		currentPreferences = new TransitiveRelation(nrAlts);
		createChildren(questions);
	}

	public int getAnswer() {
		return answer;
	}

	public Node[] getChildren() {
		Node[] ch = new Node[children.length];
		for (int i=0;i<children.length;i++) {
			ch[i] = (Node) children[i];
		}
		return ch;
	}

	@Override
	public String toString() {
		return "a" + Integer.toString(answer);
	}
	
}