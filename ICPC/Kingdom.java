import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedList;
import java.util.Queue;
import java.util.Scanner;

public class Kingdom {

	public static void main(String[] args) {

		Scanner sc = new Scanner(System.in);

		while (true) {

			// Number of towns
			int N = Integer.parseInt(sc.next());

			// The end of the input is indicated by N == 0
			if (N == 0)
				break;

			// Number of soldiers
			int G = Integer.parseInt(sc.next());

			// Number of roads
			int E = Integer.parseInt(sc.next());

			// list of all roads
			ArrayList<Double> responseTimes = new ArrayList<Double>(E);

			// Weighted adjacency matrix for APSP
			int[][] d = new int[N + 2][N + 2];
			for (int i = 0; i < N + 2; i++) {
				for (int j = 0; j < i; j++) {
					d[i][j] = Integer.MAX_VALUE / 2;
					d[j][i] = Integer.MAX_VALUE / 2;
				}
			}

			// 0,1-adjacency matrix for max flow
			int[][] adjacencyMatrix = new int[N + 2][N + 2];

			// Create roads from input
			for (int i = 0; i < E; i++) {
				int A = Integer.parseInt(sc.next());
				int B = Integer.parseInt(sc.next());
				int travelTime = Integer.parseInt(sc.next());

				// Map city 1 to N and city 2 to N+1
				if (A == 95050) {
					A = N;
				} else if (A == 104729) {
					A = N + 1;
				}
				if (B == 95050) {
					B = N;
				} else if (B == 104729) {
					B = N + 1;
				}

				d[A][B] = travelTime;
				d[B][A] = travelTime;
				adjacencyMatrix[A][B] = 1;
				adjacencyMatrix[B][A] = 1;
			}

			// Is controlling the goat cheese traffic possible at all?
			if (G < ff(adjacencyMatrix, N, N + 1, N + 2)) {
				System.out.println("Impossible");
				continue;
			}

			// Floyd-Warshall (APSP)
			for (int k = 0; k < N + 2; k++)
				for (int i = 0; i < N + 2; i++)
					for (int j = 0; j < N + 2; j++)
						if (d[i][j] > d[i][k] + d[k][j])
							d[i][j] = d[i][k] + d[k][j];

			// Matrix holding road response times
			double[][] responseTimeMatrix = new double[N + 2][N + 2];
			for (int i = 0; i < N + 2; i++) {
				for (int j = 0; j < N + 2; j++) {
					responseTimeMatrix[i][j] = Double.MAX_VALUE;
				}
			}

			// Calculate responseTime for all roads
			for (int i = 0; i < N + 2; i++) {
				for (int j = 0; j < i; j++) {

					// Travel time between the two locations
					int distLocations = d[i][j];

					// Proceed if there exists a direct road
					if (adjacencyMatrix[i][j] > 0) {

						int cityOneToI = d[i][N];
						int cityTwoToI = d[i][N + 1];
						int cityOneToJ = d[j][N];
						int cityTwoToJ = d[j][N + 1];

						double min = Double.MAX_VALUE;

						// Is town i closer to both cities?
						min = Math.min(min, Math.max(cityOneToI, cityTwoToI));

						// Is town j closer to both cities?
						min = Math.min(min, Math.max(cityOneToJ, cityTwoToJ));

						// Is city 1 closer to i and city 2 closer to j?
						if (cityOneToI < cityOneToJ && cityTwoToI > cityTwoToJ) {
							min = Math.min(min, Math.max(Math.max((cityOneToI
									+ distLocations + cityTwoToJ) / 2.0,
									cityOneToI), cityTwoToJ));
						}

						// Is city 2 closer to i and city 1 closer to j?
						if (cityOneToI > cityOneToJ && cityTwoToI < cityTwoToJ) {
							min = Math.min(min, Math.max(Math.max((cityTwoToI
									+ distLocations + cityOneToJ) / 2.0,
									cityTwoToI), cityOneToJ));
						}

						responseTimeMatrix[i][j] = min;
						responseTimeMatrix[j][i] = min;
						responseTimes.add(min);
					} else {
						responseTimeMatrix[i][j] = Double.MAX_VALUE;
						responseTimeMatrix[j][i] = Double.MAX_VALUE;
					}
				}
			}

			// Sort all road response times in acending order
			Collections.sort(responseTimes);

			// Set initial borders for binary search
			int leftBorder = 0;
			int rightBorder = E - 1;
			int oldLeftBorder = -1;
			int oldRightBorder = -1;

			// Set the maximal response time
			int maxResponseTime = (int) (responseTimes.get(E - 1) + 1.0);

			// Matrix for max flow
			int[][] currentFlowGraph = new int[N + 2][N + 2];

			// Start binary search for minimum response time
			while (true) {

				// Get current position and response time we want to check
				int currentPosition = (leftBorder + rightBorder) / 2;
				double currentResponseTime = responseTimes.get(currentPosition);

				// If both borders are equal, we're done
				if (leftBorder == rightBorder) {
					System.out.println(currentResponseTime);
					break;
				}

				// If the borders didn't change in the last run, the right one
				// holds the solution we're looking for
				if (leftBorder == oldLeftBorder
						&& rightBorder == oldRightBorder) {
					leftBorder = leftBorder + 1;
					continue;
				}

				// Build flow network
				for (int i = 0; i < N + 2; i++) {
					for (int j = 0; j < i; j++) {

						// Locations have to be connected by a road
						if (adjacencyMatrix[i][j] == 1) {

							// If a road has a higher response time than the
							// time we're currently checking, set a big
							// capacity, else, set capacity to 1
							if (responseTimeMatrix[i][j] > currentResponseTime) {
								currentFlowGraph[i][j] = maxResponseTime;
								currentFlowGraph[j][i] = maxResponseTime;
							} else {
								currentFlowGraph[i][j] = 1;
								currentFlowGraph[j][i] = 1;
							}
						} else {
							currentFlowGraph[i][j] = 0;
							currentFlowGraph[j][i] = 0;
						}
					}
				}

				// Use Ford-Fulkerson to get max flow = min cut
				int currentMaxFlow = ff(currentFlowGraph, N, N + 1, N + 2);

				// Update borders
				oldLeftBorder = leftBorder;
				oldRightBorder = rightBorder;

				// Are there enough soldiers to guard the road at the current
				// response time?
				if (currentMaxFlow > G) {
					leftBorder = currentPosition;
				} else {
					rightBorder = currentPosition;
				}
			}
		}

		sc.close();
	}

	// Breadth first search for Ford-Fulkerson 
	public static boolean bfs(int[][] rGraph, int s, int t, int parent[], int V) {
		boolean[] visited = new boolean[V];
		Queue<Integer> q = new LinkedList<Integer>();
		q.offer(s);
		visited[s] = true;
		parent[s] = -1;
		while (!q.isEmpty()) {
			int u = q.peek();
			q.poll();
			for (int v = 0; v < V; v++) {
				if (visited[v] == false && rGraph[u][v] > 0) {
					q.offer(v);
					parent[v] = u;
					visited[v] = true;
				}
			}
		}
		return (visited[t] == true);
	}
	
	// Ford-Fulkerson algorithm for finding max flow
	public static int ff(int[][] graph, int s, int t, int V) {
		int u, v;
		int[][] rGraph = new int[V][V];
		for (u = 0; u < V; u++)
			for (v = 0; v < V; v++)
				rGraph[u][v] = graph[u][v];
		int[] parent = new int[V];
		int max_flow = 0;
		while (bfs(rGraph, s, t, parent, V)) {
			int path_flow = Integer.MAX_VALUE;
			for (v = t; v != s; v = parent[v]) {
				u = parent[v];
				path_flow = Math.min(path_flow, rGraph[u][v]);
			}
			for (v = t; v != s; v = parent[v]) {
				u = parent[v];
				rGraph[u][v] -= path_flow;
				rGraph[v][u] += path_flow;
			}
			max_flow += path_flow;
		}
		return max_flow;
	}
}