// Racing Car Trail - ACM ICPC Live Archive 5882
// Accepted - Run Time 9.815 - Time Limit 40.000

import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Queue;
import java.util.Scanner;
import java.util.Set;

public class RacingCarTrail {
	
	public static void main(String[] args) {
	
		Scanner sc = new Scanner(System.in);
		
		while (true) {
			
			int N = Integer.parseInt(sc.next());
			int E = Integer.parseInt(sc.next());
			
			if (N == 0 && E == 0) {
				break;
			}
			
			sc.nextLine();
			
			Vertex[][] map = new Vertex[N+1][E+1];
			Set<Vertex> U = new HashSet<Vertex>();
			Set<Vertex> V = new HashSet<Vertex>();
			Set<Edge> edges = new HashSet<Edge>();
			
			for (int i = 1; i < N+1; i++) {
				
				String inputLine = sc.nextLine();
				
				for (int j = 1; j < E+1; j++) {
					if (inputLine.charAt(j-1) == '.') {
						map[i][j] = new Vertex();
						map[i][j].id = (i-1)*E+(j-1);
						
						if (i % 2 == 0 && j % 2 == 0 || i % 2 == 1 && j % 2 == 1)
							U.add(map[i][j]);
						else
							V.add(map[i][j]);
						
						if (map[i-1][j] != null) {
							Edge e = new Edge(map[i][j], map[i-1][j]);
							edges.add(e);
							map[i][j].edges.add(e);
							map[i-1][j].edges.add(e);
						}
						
						if (map[i][j-1] != null) {
							Edge e = new Edge(map[i][j], map[i][j-1]);
							edges.add(e);
							map[i][j].edges.add(e);
							map[i][j-1].edges.add(e);
						}
					}
				}
			}
			
			Set<Edge> matching = hopcroftKarp(U,V,edges);
			for (Edge e : matching) {
				e.inMatching = true;
				e.u.isFree = false;
				e.v.isFree = false;
			}
			
			for (int i = 1; i < N+1; i++) {
				for (int j = 1; j < E+1; j++) {
					Vertex x = map[i][j];
					
					if (x == null) {
						System.out.print("X");
						continue;
					}
					
					if (x.isFree) {
						System.out.print("B");
						continue;
					}
					
					if (dfsForAugmentation(x, new LinkedList<Vertex>(), new boolean[N*E], 0, matching) == 0) {
						System.out.print("B");
					} else {
						System.out.print("A");
					}
				}
				System.out.println();
			}
			
			System.out.println();
		}
		
		sc.close();
	}
	
	public static int dfsForAugmentation(Vertex x, LinkedList<Vertex> path, boolean[] visited, int turn, Set<Edge> matching) {
		
		if (path.size() == 0) {
			path.add(x);
			visited[x.id] = true;
		}
		if (x.isFree) {
			return turn;
		}

		for (Edge e : x.edges) {
			Vertex v = e.giveAdjacentVertex(x);
			
			if (turn == 0 && !matching.contains(e) || turn == 1 && matching.contains(e))
				continue;
			
			
			if (!visited[v.id]) {
				path.add(v);
				visited[v.id] = true;
				
				int tmp = dfsForAugmentation(v, path, visited, (turn+1)%2, matching);
				
				if (tmp > -1) {
					return tmp;
				} else {
					path.remove(path.size()-1);
				}
			} 
		}
		return -1;
		
	}
	
	public static boolean bfs(Set<Vertex> U, Set<Vertex> V, Vertex nil, HashMap<Vertex,Vertex> pairU, HashMap<Vertex,Vertex> pairV) {
		
		Queue<Vertex> Q = new LinkedList<Vertex>(); 
		
		for (Vertex v : U) {
			if (pairU.get(v) == nil) {
				v.dist = 0;
				Q.offer(v);
			} else {
				v.dist = Integer.MAX_VALUE;
			}
		}
		nil.dist = Integer.MAX_VALUE;
		
		while (!Q.isEmpty()) {
			Vertex v = Q.poll();
			if (v.dist < nil.dist) {
				for (Edge e : v.edges) {
					Vertex u = e.giveAdjacentVertex(v);
					if (pairV.get(u).dist == Integer.MAX_VALUE) {
						pairV.get(u).dist = v.dist + 1;
						Q.offer(pairV.get(u));
					}
				}
			}
		}
		
		return nil.dist != Integer.MAX_VALUE;
	}
	
	public static boolean dfs(Vertex v, Vertex nil, HashMap<Vertex,Vertex> pairU, HashMap<Vertex,Vertex> pairV) {
		if (v != nil) {
			for (Edge e : v.edges) {
				Vertex u = e.giveAdjacentVertex(v);
				if (pairV.get(u).dist == v.dist + 1) {
					if (dfs(pairV.get(u),nil,pairU,pairV)) {
						pairV.put(u,v);
						pairU.put(v,u);
						return true;
					}
				}
			}
			v.dist = Integer.MAX_VALUE;
			return false;
		}
		return true;
	}
	
	public static Set<Edge> hopcroftKarp(Set<Vertex> U, Set<Vertex> V, Set<Edge> edges) {
		
		Set<Edge> matching = new HashSet<Edge>();
		
		Vertex nil = new Vertex();
		HashMap<Vertex,Vertex> pairU = new HashMap<Vertex,Vertex>();
		HashMap<Vertex,Vertex> pairV = new HashMap<Vertex,Vertex>();
		
		for (Vertex u : U)
			pairU.put(u,nil);
		
		for (Vertex v : V)
			pairV.put(v,nil);
		
		while(bfs(U, V, nil, pairU, pairV)) {
			for (Vertex u : U) {
				if (pairU.get(u) == nil) {
					dfs(u,nil,pairU,pairV);
				}
			}
		}
		
		for (Edge e : edges) {
			if ((pairU.containsKey(e.u) && pairU.get(e.u) == e.v) || (pairV.containsKey(e.u) && pairV.get(e.u) == e.v) ||
					(pairU.containsKey(e.v) && pairU.get(e.v) == e.u) || (pairV.containsKey(e.v) && pairV.get(e.v) == e.u)) {
				matching.add(e);
			}
		}
		
		return matching;
	}
}

class Vertex {
	int id;
	boolean isFree;
	LinkedList<Edge> edges;
	int dist;

	public Vertex() {
		this.id = 0;
		this.isFree = true;
		this.edges = new LinkedList<Edge>();
		this.dist = Integer.MAX_VALUE;
	}

	@Override
	public String toString() {
		return "" + id;
	}
}

class Edge {
	Vertex u;
	Vertex v;
	boolean inMatching;
	
	public Edge(Vertex u, Vertex v) {
		this.u = u;
		this.v = v;
		this.inMatching = false;
	}
	
	public Vertex giveAdjacentVertex(Vertex x) {
		if (x == u)
			return v;
		if (x == v)
			return u;
		return null;
	}
	
	@Override
	public String toString() {
		return "(" + u.id + "," + v.id + ")";
	}
}