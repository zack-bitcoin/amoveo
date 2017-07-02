## FOR INVESTIGATION:
## THIS TESTS IS CONFLICTING WITH test_fork.py and tests.py (if you run any of these, test_lightning.py will fail)


# # this quickly tests lightning payments. It is a lot faster and easier than using the blockchain to test the same thing.
#
# # It will use test master key and corresponding configs
#
# # It lightning spends 4 tokens one way, then spends the same 4 back.
#
# from time import sleep
#
# from base import TestBase
#
#
# class LightningTest(TestBase):
#     def test_payments(self):
#         response = self.session.post("http://localhost:3011",
#                                      data='["sync", [127,0,0,1], 3020]')
#         self.assertEqual(response.status_code, 200)
#         response = self.session.post("http://localhost:3011",
#                                      data='["sync", [127,0,0,1], 3030]')
#         self.assertEqual(response.status_code, 200)
#         sleep(1)
#
#         response = self.session.post("http://localhost:3011",
#                                      data='["create_account", "OGlqQmhFUks0anBSVHp5Y1lDZGtVTjh0MWg5UDg2YTExMWs3N0RTUDZadUht", 10]')
#         self.assertEqual(response.status_code, 200)
#         sleep(1)
#         response = self.session.post("http://localhost:3011",
#                                      data='["create_account", "MjFtZk5oeFFNWmphcVFzZzdVTHRZMTlQblN4b2dIQVRkSHg2SjRrSEF2MWdBag==", 10]')
#         self.assertEqual(response.status_code, 200)
#         sleep(1)
#
#         response = self.session.post("http://localhost:3011",
#                                      data='["sync", [127,0,0,1], 3030]')
#         self.assertEqual(response.status_code, 200)
#         sleep(1)
#         response = self.session.post("http://localhost:3021",
#                                      data='["sync", [127,0,0,1], 3030]')
#         self.assertEqual(response.status_code, 200)
#         sleep(1)
#
#         # 2 step handshake to make channel
#         response = self.session.post("http://localhost:3011",
#                                      data='["new_channel_with_server", [127,0,0,1], 3030, 1, 10000, 10001, 50, 4]')
#         self.assertEqual(response.status_code, 200)
#         sleep(0.5)
#         response = self.session.post("http://localhost:3021",
#                                      data='["sync", [127,0,0,1], 3030]')
#         self.assertEqual(response.status_code, 200)
#         sleep(0.1)
#
#         response = self.session.post("http://localhost:3021",
#                                      data='["new_channel_with_server", [127,0,0,1], 3030, 2, 10000, 10001, 50, 4]')
#         self.assertEqual(response.status_code, 200)
#         response = self.session.post("http://localhost:3011",
#                                      data='["sync", [127,0,0,1], 3030]')
#         self.assertEqual(response.status_code, 200)
#         sleep(1)
#
#         response = self.session.post("http://localhost:3011",
#                                      data='["channel_spend", [127,0,0,1], 3030, 777]')
#         self.assertEqual(response.status_code, 200)
#         sleep(1)
#
#         response = self.session.post("http://localhost:3011",
#                                      data='["lightning_spend", [127,0,0,1], 3030, 2, "BAiwm5uz5bLkT+Lr++uNI02jU3Xshwyzkywk0x0ARwY5j4lwtxbKpU+oDK/pTQ1PLz7wyaEeDZCyjcwt9Foi2Ng=", 4, 10]')
#         self.assertEqual(response.status_code, 200)
#         sleep(1)
#
#         response = self.session.post("http://localhost:3021",
#                                      data='["pull_channel_state", [127,0,0,1], 3030]')
#         self.assertEqual(response.status_code, 200)
#         sleep(1)
#
#         response = self.session.post("http://localhost:3011",
#                                      data='["pull_channel_state", [127,0,0,1], 3030]')
#         self.assertEqual(response.status_code, 200)
#         sleep(1)
#
#         response = self.session.post("http://localhost:3021",
#                                      data='["lightning_spend", [127,0,0,1], 3030, 1, "BIVZhs16gtoQ/uUMujl5aSutpImC4va8MewgCveh6MEuDjoDvtQqYZ5FeYcUhY/QLjpCBrXjqvTtFiN4li0Nhjo=", 4, 10]')
#         self.assertEqual(response.status_code, 200)
#         sleep(1)
#
#         response = self.session.post("http://localhost:3011",
#                                      data='["pull_channel_state", [127,0,0,1], 3030]')
#         self.assertEqual(response.status_code, 200)
#         sleep(1)
#
#         response = self.session.post("http://localhost:3021",
#                                      data='["pull_channel_state", [127,0,0,1], 3030]')
#         self.assertEqual(response.status_code, 200)
